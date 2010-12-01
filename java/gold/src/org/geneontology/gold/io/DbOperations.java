package org.geneontology.gold.io;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.factory.GoldDeltaFactory;
import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ClsIntersectionOf;
import org.geneontology.gold.hibernate.model.ClsUnionOf;
import org.geneontology.gold.hibernate.model.DisjointWith;
import org.geneontology.gold.hibernate.model.EquivalentTo;
import org.geneontology.gold.hibernate.model.InferredAllSomeRelationship;
import org.geneontology.gold.hibernate.model.InferredSubclassOf;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.ObjDefinitionXref;
import org.geneontology.gold.hibernate.model.ObjXref;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.hibernate.Session;
import org.obolibrary.obo2owl.Obo2Owl;

import owltools.graph.OWLGraphWrapper;

/**
 * This class provides adminstrative operations for the GOLD database.
 * @author Shahid Manzoor
 *
 */
public class DbOperations {

	
	private List<DbOperationsListener> listeners;
	
	private static Logger LOG = Logger.getLogger(DbOperations.class);

	public DbOperations(){
		listeners = new ArrayList<DbOperationsListener>();
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
		String oboFile = GeneOntologyManager.getInstance().getDefaultOboFile();
		
		if(oboFile == null){
			throw new FileNotFoundException("Obo File not Found at the path '" + 
					GeneOntologyManager.getInstance().getProperty("geneontology.gold.obofile") + "'");
		}
		
		bulkLoad(oboFile, force);
	}
	
	/**
	 * Load the contents of the obo file into GOLD
	 * @param oboFile: The path of the obo file
	 * @param The true value of the force parameter drops all the existing tables
	 * 			creates new ones.
	 * @throws Exception
	 */
	public void bulkLoad(String oboFile, boolean force) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
 		
		if(LOG.isDebugEnabled()){
			LOG.debug("Bulk Load for: " + oboFile);
		}
		

		List<String> list = dumpFiles("", oboFile);
		buildSchema(force, "");
		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
		
		LOG.info("Bulk Load completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
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
	public List<String> dumpFiles(String tablePrefix, String oboFile) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		OWLGraphWrapper wrapper = new OWLGraphWrapper( 	
			new Obo2Owl().convert(oboFile));
		
		OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, manager.getTsvFilesDir(), tablePrefix);
		
		List<String> list = loader.dumpBulkLoadTables();
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;
		
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
				manager.getOntSqlSchemaFileLocation(), tablePrefix, force);
		
		
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
	public void updateGold() throws Exception{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}

		updateGold(GeneOntologyManager.getInstance().getDefaultOboFile());
	}	
	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void updateGold(String oboFile) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.updateStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}

		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		
		List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), oboFile);
		buildSchema(true, manager.getGoldDetlaTablePrefix());

		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
		
		GoldDeltaFactory gdf = new GoldDeltaFactory();

		if(LOG.isDebugEnabled()){
			LOG.debug("Extracting delt hibernate objects from prefixed temporary tables");
		}
		
		List<SubclassOf> subclassList = gdf.buildSubclassOfDelta();
		List<Relation> relationList = gdf.buildRelationDelta();
		List<AllSomeRelationship> asmList = gdf.buildAllSomeRelationships();
		List<Cls> clsList = gdf.buildClsDelta();
		List<ObjAlternateLabel> oalList = gdf.buildObjAlternateLabels();
		List<ObjXref> xrefList = gdf.buildObjXrefs();
		List<ObjDefinitionXref> defXrefList = gdf.buildObjDefinitionXref();
		List<EquivalentTo> eqList = gdf.buildEquivalentTo();
		List<ClsUnionOf> unList = gdf.buildClsUnionOf();
		List<ClsIntersectionOf> intList = gdf.buildClsIntersectionOf();
		List<InferredSubclassOf> infSubList = gdf.buildInferredSubclassOf();
		List<InferredAllSomeRelationship> infSomeList = gdf.buildInferredAllSomeRelationship();
//		List<DisjointWith> djList = gdf.buildDisjointWith();
		
		//close the session associated with the tables prefixed with 
		// the value of the geneontology.gold.deltatableprefix property
		gdf.getSession().close();
		
		if(LOG.isDebugEnabled()){
			LOG.debug("Merging the the delta objects in the GOLD database");
		}
		
		
		GoldObjectFactory gof = GoldObjectFactory.buildDefaultFactory();
		Session session = gof.getSession();
		session.clear();

		saveList(session, clsList);
		
		saveList(session, relationList);
		
		saveList(session, asmList);
		
		saveList(session, clsList);

		saveList(session, subclassList);
		
		saveList(session, oalList);
		
		saveList(session, xrefList);
		
		saveList(session, defXrefList);
		
		saveList(session, eqList);
		
	//	saveList(session, djList);
		
		saveList(session, unList);
		
		saveList(session, intList);
		
		saveList(session, infSubList);
		
		saveList(session, infSomeList);
		
		LOG.info("Database update is completed");
		
		session.getTransaction().commit();
		
		for(DbOperationsListener listener: listeners){
			listener.updateEnd();
		}
		
		
	}
	
	
	public void addDbOperationsListener(DbOperationsListener listener){
		listeners.add(listener);
	}

	public void removeDbOperationsListener(DbOperationsListener listener){
		listeners.remove(listener);
	}
	
	private void saveList(Session session, List list){
		for(Object obj: list){
			session.saveOrUpdate(obj);
		}
	}
	
}
