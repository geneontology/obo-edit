package org.geneontology.gaf.io;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.CompositeQualifier;
import org.geneontology.gaf.hibernate.ExtensionExpression;
import org.geneontology.gaf.hibernate.GafDeltaFactory;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.hibernate.WithInfo;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperationsInterface;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.hibernate.Session;

public class GAFDbOperations implements DbOperationsInterface{

	private List<DbOperationsListener> listeners;
	
	private static Logger LOG = Logger.getLogger(GAFDbOperations.class);

	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private boolean dbCreate;
	
	private GafDocument gafDocument;
	
	public GAFDbOperations(){
		listeners = new ArrayList<DbOperationsListener>();
	}

	
	
	public List<Ontology> getLastUpdateStatus(){
		return null;
	}
	
	
	/**
	 * Loads the contents of the obo file whose path is supplied 
	 * the geneontology.gold.obofil property.
	 * @param force
	 * @throws Exception
	 */
	public void bulkLoad(boolean force) throws Exception{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		List files = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
		
		if(files == null || files.size()==0){
			throw new Exception("Ontology File Location is not Found specified in the geneontology.gold.ontologylocation property" );
		}
		
		dbCreate = false;
		
		for(Object obj: files){
			bulkLoad(obj.toString(), force);
		}
	}
	
	/**
	 * Load the contents of the obo file into GOLD
	 * @param oboFile: The path of the obo file
	 * @param The true value of the force parameter drops all the existing tables
	 * 			creates new ones.
	 * @throws Exception
	 */
	public void bulkLoad(String gafLocation, boolean force) throws Exception{

		gafDocument = buildGafDocument(gafLocation);
		
		bulkLoad(gafDocument, force);
	
	}
	
	public void bulkLoad(GafDocument gafDocument, boolean force) throws Exception{
		if(DEBUG)
			LOG.debug("--");
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
		
		
		List<String> list = dumpFiles("", gafDocument);
		
		if(!dbCreate)
			buildSchema(force, "");
		
		dbCreate = true;
		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);

		GafObjectsFactory factory = new GafObjectsFactory();
		Session session = factory.getSession();
		
		bulkLoadHibernate(session, gafDocument);
	
		session.getTransaction().commit();
		
		LOG.info("Bulk Load completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
		}
		
	}
	
	private void bulkLoadHibernate(Session session, GafDocument gafDocument){

		
		for(String id: gafDocument.getCompositeQualifiersIds()){
			for(CompositeQualifier cq: gafDocument.getCompositeQualifiers(id)){
				session.saveOrUpdate(cq);
			}
		}
		
		for(String id: gafDocument.getWithInfosIds()){
			List<WithInfo> list = gafDocument.getWithInfos(id);
			for(WithInfo wi: list){
				session.saveOrUpdate(wi);
			}
		}
		
		for(String id: gafDocument.getExtensionExpressionIds()){
			for(ExtensionExpression ex: gafDocument.getExpressions(id)){
				session.saveOrUpdate(ex);
			}
		}
		
	}
	
	
	/**
	 * This method dumps the obo file (oboFile) as tab separated files
	 * for GOLD database to be used for bulk loading 
	 * @param tablePrefix
	 * @param oboFile
	 * @return It returns the name of the tables for which the files are dumped
	 * @throws Exception
	 */
	public List<String> dumpFiles(String tablePrefix, String gafFile) throws Exception{
		/*for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();

		gafDocument = buildGafDocument(gafFile);
		GafBulkLoader loader = new GafBulkLoader(gafDocument, manager.getTsvFilesDir(), tablePrefix);
		
		List<String> list = loader.loadAll();
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;*/
		
		gafDocument = buildGafDocument(gafFile);
		return dumpFiles(tablePrefix, gafDocument);
	}

	public List<String> dumpFiles(String tablePrefix, GafDocument gafDocument) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();

		GafBulkLoader loader = new GafBulkLoader(gafDocument, manager.getTsvFilesDir(), tablePrefix);
		
		List<String> list = loader.loadAll();
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;
		
	}
	
	public GafDocument buildGafDocument(Reader reader) throws IOException{
		for(DbOperationsListener listener: listeners){
			listener.startOntologyLoad();
		}

		GafObjectsBuilder builder = new GafObjectsBuilder();
		
		GafDocument doc = builder.buildDocument(reader);

		for(DbOperationsListener listener: listeners){
			listener.endOntologyLoad(builder);
		}
		
		return doc;

	}
	public GafDocument buildGafDocument(String locaiton) throws IOException{
		FileReader reader = new FileReader(new File(locaiton));
		return buildGafDocument(reader);
	}
	
	
	/**
	 * It creates schema of GOLD database.
	 * @param force: The true value of this parameter drop of all existing tables and
	 * 			creates new ones.
	 * @param tablePrefix This prefix is used as prefix of each table created in the database.
	 * @throws Exception
	 */
	public void buildSchema(boolean force, String tablePrefix) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.buildSchemaStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}

		SchemaManager sm = new SchemaManager();
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		sm.loadSchemaSQL(manager.getGolddbHostName(),
				manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbName(),
				manager.getGafSqlSchemaFileLocation(), tablePrefix, force);
		
		
		for(DbOperationsListener listener: listeners){
			listener.buildSchemaEnd();
		}
		
	}
	
	/**
	 * It loads the TSV files in the GOLD database.
	 * @param tsvFilesDir The directory where TSV files are residing
	 * @param list It is list of the names (without extension) of the files 
	 * 		to be loaded in the GOLD database
	 * @throws Exception
	 */
	public void loadTsvFiles(String tsvFilesDir, List<String> list) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.loadTsvFilesStart();
		}
	
		
		if(LOG.isDebugEnabled()){
			LOG.debug(list + " files being loaded");
		}

		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		TsvFileLoader tsvLoader = new TsvFileLoader(manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
				manager.getGolddbName());
		
		tsvLoader.loadTables(tsvFilesDir, list);
		
		
		LOG.info("TSV files load completed");
		
		for(DbOperationsListener listener: listeners){
			listener.loadTsvFilesEnd();
		}
		
	}

	/**
	 * Loads all files with extension .txt specified at the path tsvFilesDir
	 * @param tsvFilesDir
	 * @throws Exception
	 */
	public void loadTsvFiles(String tsvFilesDir) throws Exception{
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}

		File dir = new File(tsvFilesDir);
		
		File[] files = dir.listFiles(new FileFilter() {
			
//			@Override
			public boolean accept(File file) {
				return file.getName().endsWith(".txt");
			}
		});
		
		List<String> list = new ArrayList<String>();
		
		for(File f: files){
			list.add(f.getName().subSequence(0, f.getName().length()-4).toString());
		}
		
		loadTsvFiles(tsvFilesDir, list);
		
	}


	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located the default location.
	 * @throws Exception
	 */
	public void update() throws Exception{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		List list = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
		
		if(list == null || list.size()==0){
			throw new Exception("Gaf File Locations are not specified in the geneontology.gold.gaflocation property" );
		}
		
		for(Object obj: list)
			update(obj.toString());
		

	}	
	
	public void update(String gafLocation) throws Exception{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		gafDocument = buildGafDocument(gafLocation);
		
		update(gafDocument);
	}

	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void update(GafDocument gafDocument) throws Exception{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		
		List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), gafDocument);

		buildSchema(true, manager.getGoldDetlaTablePrefix());

		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
		
		GafDeltaFactory deltaFactory = new GafDeltaFactory(gafDocument);
		
		Collection<Bioentity> entities = deltaFactory.buildBioentityDelta();
		Collection<GeneAnnotation> annotations = deltaFactory.buildGeneAnnotations();
		Collection<CompositeQualifier> qualifiers = deltaFactory.buildCompositeQualifiers();
		Collection<ExtensionExpression> expressions = deltaFactory.buildExtensionExpressions();
		Collection<WithInfo> infos = deltaFactory.buildWithInfos();
		
		GafObjectsFactory factory = new GafObjectsFactory();
		Session session = factory.getSession();
		
		saveOrUpdate(session, entities);
		saveOrUpdate(session, annotations);
		saveOrUpdate(session, qualifiers);
		saveOrUpdate(session, expressions);
		saveOrUpdate(session, infos);
		
	
		session.getTransaction().commit();
		
		LOG.info("Bulk Load completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
		}
		
		
	}
	
	private void saveOrUpdate(Session session, Collection objects){
		
		for(Object obj: objects){
			session.saveOrUpdate(obj);
		}
	}
	
	public void addDbOperationsListener(DbOperationsListener listener){
		if(!listeners.contains(listener))
			listeners.add(listener);
	}

	public void removeDbOperationsListener(DbOperationsListener listener){
		listeners.remove(listener);
	}
	
	
}
