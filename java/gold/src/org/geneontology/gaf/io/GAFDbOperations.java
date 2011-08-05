package org.geneontology.gaf.io;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.zip.GZIPInputStream;
import org.apache.log4j.Logger;
import org.eclipse.jetty.util.log.Log;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.CompositeQualifier;
import org.geneontology.gaf.hibernate.ExtensionExpression;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.hibernate.WithInfo;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.hibernate.Session;

public class GAFDbOperations{

	private List<DbOperationsListener> listeners;
	
	private static Logger LOG = Logger.getLogger(GAFDbOperations.class);

	private static boolean DEBUG = LOG.isDebugEnabled();
	
	//private boolean dbCreate;
	
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
	public void bulkLoad(boolean force) throws GafDbOperationsException{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		List files = GoConfigManager.getInstance().getDefaultGafFileLocations();
		
		if(files == null || files.size()==0){
			throw new GafDbOperationsException("Ontology File Location is not Found specified in the geneontology.gold.ontologylocation property" );
		}
		
	//	dbCreate = false;
		
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
	public void bulkLoad(String gafLocation, boolean force) throws GafDbOperationsException{

		try{
			gafDocument = buildGafDocument(gafLocation);
		}catch(IOException ex){
			throw new GafDbOperationsException("An Error occured while building GAF document", ex);
		}
		
		bulkLoad(gafDocument, force);
	
	}
	
	public void bulkLoad(GafDocument gafDocument, boolean force) throws GafDbOperationsException{
		if(DEBUG)
			LOG.debug("--");
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
		
		
		List<String> list = dumpFiles("", gafDocument);
		
		/*if(!dbCreate)
			buildSchema(force, "");
		
		dbCreate = true;*/
		loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), list);

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

		
		session.save(gafDocument);
		
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
	public List<String> dumpFiles(String tablePrefix, String gafFile) throws GafDbOperationsException{
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
		
		try{
			gafDocument = buildGafDocument(gafFile);
		}catch(IOException ex){
			throw new GafDbOperationsException("An Error occured while building GAF document", ex);
		}
			

		return dumpFiles(tablePrefix, gafDocument);
	}

	public List<String> dumpFiles(String tablePrefix, GafDocument gafDocument) throws GafDbOperationsException{
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		GoConfigManager manager = GoConfigManager.getInstance();

		GafBulkLoader loader = new GafBulkLoader(gafDocument, manager.getTsvFilesDir(), tablePrefix);
		
		List<String> list = null;
			
		try{
			list= loader.loadAll();
		}catch(IOException ex){
			throw new GafDbOperationsException(ex);
		}
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;
		
	}
	
	public GafDocument buildGafDocument(Reader reader, String docId, String path) throws IOException{
		for(DbOperationsListener listener: listeners){
			listener.startDomLoad();
		}

		GafObjectsBuilder builder = new GafObjectsBuilder();
		
		GafDocument doc = builder.buildDocument(reader, docId, path);

		for(DbOperationsListener listener: listeners){
			listener.endDomLoad(builder);
		}
		
		return doc;

	}
	public GafDocument buildGafDocument(String locaiton) throws IOException{
		LOG.info("Loading GAF document from the '" + locaiton + "' location.");
		File f = new File(locaiton);		
		
		InputStream is = new FileInputStream(f);
		
		if(locaiton.endsWith(".gz")){
			is = new GZIPInputStream(is);
		}
		
		return buildGafDocument(new InputStreamReader(is), f.getName(), f.getCanonicalPath());
	}
	
	
	/**
	 * It creates schema of GOLD database.
	 * @param force: The true value of this parameter drop of all existing tables and
	 * 			creates new ones.
	 * @param tablePrefix This prefix is used as prefix of each table created in the database.
	 * @throws Exception
	 */

	public void buildSchema(boolean force, String tablePrefix) throws GafDbOperationsException{
		for(DbOperationsListener listener: listeners){
			listener.buildSchemaStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}

		SchemaManager sm = new SchemaManager();
		GoConfigManager manager = GoConfigManager.getInstance();
		
		try{
			sm.loadSchemaSQL(manager.getGolddbHostName(),
					manager.getGolddbUserName(),
					manager.getGolddbUserPassword(), manager.getGolddbName(),
					manager.getGafSqlSchemaFileLocation(), tablePrefix, force);
		}catch(Exception ex){
			throw new GafDbOperationsException(ex);
		}
		
		
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
	public void loadTsvFiles(String tsvFilesDir, List<String> list) throws GafDbOperationsException{
		for(DbOperationsListener listener: listeners){
			listener.loadTsvFilesStart();
		}
	
		
		if(LOG.isDebugEnabled()){
			LOG.debug(list + " files being loaded");
		}

		GoConfigManager manager = GoConfigManager.getInstance();

		try{
			TsvFileLoader tsvLoader = new TsvFileLoader(manager.getGolddbUserName(),
					manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
					manager.getGolddbName());
			
			tsvLoader.loadTables(tsvFilesDir, list);
		}catch(Exception ex){
			throw new GafDbOperationsException(ex);
		}
		
		
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
	public void loadTsvFiles(String tsvFilesDir) throws GafDbOperationsException{
		
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
	public void update() throws GafDbOperationsException{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		List list = GoConfigManager.getInstance().getDefaultGafFileLocations();
		
		if(list == null || list.size()==0){
			throw new GafDbOperationsException("Gaf File Locations are not specified in the geneontology.gold.gaflocation property" );
		}
		
		for(Object obj: list)
			update(obj.toString());
		

	}	
	
	public void update(String gafLocation) throws GafDbOperationsException{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		try{
			gafDocument = buildGafDocument(gafLocation);
		}catch(IOException ex){
			throw new GafDbOperationsException("An Error occured while building GAF document", ex);
		}
		
		update(gafDocument);
	}

	
	/**
	 * Incremental update of the GOLD database from the contents of the GAF document
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void update(GafDocument gafDocument) throws GafDbOperationsException{
		update(gafDocument, false);
	}
	
	/**
	 * This variable is set to true only when a big document loaded
	 * through split methodology
	 */
	private boolean isSchemaCreted;
	
	public void update(GafDocument gafDocument, boolean splitt) throws GafDbOperationsException{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		if(gafDocument != null){
		
			this.gafDocument = gafDocument;
			/*if(factory.getGafDocument().isEmpty()){
				bulkLoad(gafDocument, false);
				return;
			}*/
			
			for(DbOperationsListener listener: listeners){
				listener.updateStart();
			}
			
			GoConfigManager manager = GoConfigManager.getInstance();

			if(!splitt || (splitt && !isSchemaCreted)){
				
				buildSchema(true, manager.getGoldDetlaTablePrefix());
				isSchemaCreted = true;

			}

			
			List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), gafDocument);
			
			
			List<String> ll = new ArrayList<String>();
			
			ll.add(manager.getGoldDetlaTablePrefix()+"gene_annotation");
			
			loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), ll);
			isSchemaCreted = true;
			
			Log.info("updating bioentity table.");
			
				GafObjectsFactory f = new GafObjectsFactory();
				Session ssn = f.getSession();
			
				for(Bioentity be: gafDocument.getBioentities()){
					ssn.saveOrUpdate(be);
				}
				
				for(String id: gafDocument.getWithInfosIds()){
					for(WithInfo wi: gafDocument.getWithInfos(id)){
						ssn.saveOrUpdate(wi);
					}
				}
				
				for(String id: gafDocument.getExtensionExpressionIds()){
					for(ExtensionExpression ee: gafDocument.getExpressions(id)){
						ssn.saveOrUpdate(ee);
					}
				}
				
				for(String id: gafDocument.getCompositeQualifiersIds()){
					for(CompositeQualifier cq: gafDocument.getCompositeQualifiers(id)){
						ssn.saveOrUpdate(cq);
					}
				}
				
				
				
				for(GeneAnnotation ga: gafDocument.getGeneAnnotations()){
					ssn.saveOrUpdate(ga);
				}
				
				ssn.saveOrUpdate(gafDocument);
				
				ssn.getTransaction().commit();
			
			if(splitt)
				return;
		}
		
		Log.info("last step of update.");
		isSchemaCreted = false;

//		GafDeltaFactory deltaFactory = new GafDeltaFactory(gafDocument);
//		GafObjectsFactory factory = new GafObjectsFactory();
		
//		Collection<Bioentity> entities = deltaFactory.buildBioentityDelta();
//		Collection<GeneAnnotation> annotations = deltaFactory.buildGeneAnnotations();
//		Collection<CompositeQualifier> qualifiers = deltaFactory.buildCompositeQualifiers();
//		Collection<ExtensionExpression> expressions = deltaFactory.buildExtensionExpressions();
//		Collection<WithInfo> infos = deltaFactory.buildWithInfos();
		
//		deltaFactory.closeConnection();
		
//		Session session = factory.getSession();
		
		
//		session.saveOrUpdate(this.gafDocument);
//		saveOrUpdate(session, entities);
//s		saveOrUpdate(session, annotations);
//		saveOrUpdate(session, qualifiers);
///		saveOrUpdate(session, expressions);
//		saveOrUpdate(session, infos);
		
	
	//	session.getTransaction().commit();
		
		LOG.info("Update completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.updateEnd();
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
