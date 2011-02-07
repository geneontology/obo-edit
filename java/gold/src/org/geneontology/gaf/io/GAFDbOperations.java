package org.geneontology.gaf.io;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.GAFParserHandlerForHibernate;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.parser.GAFParser;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperationsInterface;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;

public class GAFDbOperations implements DbOperationsInterface{

	private List<DbOperationsListener> listeners;
	
	private static Logger LOG = Logger.getLogger(GAFDbOperations.class);

	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private boolean dbCreate;
	
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
		if(DEBUG)
			LOG.debug(gafLocation);
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
		
		
		List<String> list = dumpFiles("", gafLocation);
		
		if(!dbCreate)
			buildSchema(force, "");
		
		dbCreate = true;
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
	public List<String> dumpFiles(String tablePrefix, String gafFile) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();

		GafBulkLoader loader = new GafBulkLoader(buildGafDocument(gafFile), manager.getTsvFilesDir(), tablePrefix);
		
		List<String> list = loader.loadAll();
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;
		
	}
	
	public GafDocument buildGafDocument(String locaiton) throws IOException{
		GAFParser parser = new GAFParser();
		GAFParserHandlerForHibernate handler = new GAFParserHandlerForHibernate();
		parser.parse(new File(locaiton), handler);
		
		return handler.getGafDocument();
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
	public void updateGold() throws Exception{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}

	}	
	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void updateGold(String oboFile) throws Exception{
		
		
	}
	
	public void addDbOperationsListener(DbOperationsListener listener){
		if(!listeners.contains(listener))
			listeners.add(listener);
	}

	public void removeDbOperationsListener(DbOperationsListener listener){
		listeners.remove(listener);
	}
	
	
}
