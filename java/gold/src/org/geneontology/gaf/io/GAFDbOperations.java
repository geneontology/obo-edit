package org.geneontology.gaf.io;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafHibObjectsBuilder;
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gold.hibernate.model.GoldObjectFactory;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.hibernate.Session;
import org.hibernate.jdbc.Work;
import owltools.gaf.Bioentity;
import owltools.gaf.CompositeQualifier;
import owltools.gaf.ExtensionExpression;
import owltools.gaf.WithInfo;

/**
 * The class performs Database operations (bulkload and update) for GAF files. 
 * @author Shahid Manzoor
 *
 */
public class GAFDbOperations{

	//Holds list of the listeners which notified with different stages of a db operation.
	private List<DbOperationsListener> listeners;
	
	private static Logger LOG = Logger.getLogger(GAFDbOperations.class);

	private static boolean DEBUG = LOG.isDebugEnabled();
	
	
	public GAFDbOperations(){
		listeners = new ArrayList<DbOperationsListener>();
	}

	
	
	/**
	 * Loads the contents of the gaf file whose path is supplied 
	 * the geneontology.gold.gaflocation property in the conf/gold.properties.
	 * @param force This parameter overrides the database tables.
	 * @throws Exception
	 */
	public void bulkLoad(boolean force) throws GafDbOperationsException{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		//Geting GAf files configured in the gold.properties file
		List files = GoConfigManager.getInstance().getDefaultGafFileLocations();
		
		if(files == null || files.size()==0){
			throw new GafDbOperationsException("Ontology File Location is not Found specified in the geneontology.gold.ontologylocation property" );
		}
		
		for(Object obj: files){
			bulkLoad(obj.toString(), force);
		}
	}
	
	/**
	 * Load the contents of the gaf file into GOLD
	 * @param oboFile: The path of the obo file. The path could be any for these http,ftp and file:// forms. It can also be relative path.
	 * @param The true value of the force parameter drops all the existing tables
	 * 			creates new ones.
	 * @throws Exception
	 */
	public void bulkLoad(String gafLocation, boolean force) throws GafDbOperationsException{

		//if path is relative the function will convert it to URI
		gafLocation = toURI(gafLocation);
		
		GafURLFetch fetch = new GafURLFetch(gafLocation);
		Reader reader = new InputStreamReader((InputStream)fetch.next());
		
		bulkload(reader, fetch.getCurrentGafFile(), fetch.getCurrentGafFilePath(), force);
	}
	
	/**
	 * Converts file location to URI
	 * @param gafLocation
	 * @return
	 * @throws GafDbOperationsException
	 */
	private String toURI(String gafLocation) throws GafDbOperationsException{
		if(gafLocation == null )
			throw new GafDbOperationsException("Gaf location is not provided in the input");
		
		gafLocation = gafLocation.trim();
		if(!(gafLocation.startsWith("http:") || gafLocation.startsWith("ftp:") || gafLocation.startsWith("file:"))){
			File f = new File(gafLocation);
			gafLocation= f.toURI().toString();
		}
		
		return gafLocation;
		
	}
	
	/**
	 * It builds {@link GafDocument} from the reader and perform bulkload on the document. If the document
	 * is large enough then it read the document in blocks sequentially and performs bulkload on each block in temporary tables.
	 * Each block is a fixed set of rows, e.g. 900000 row, in GAF file.
	 * Once all the blocks are bulk-loaded then the changes are propagated into the database.  
	 * @param reader
	 * @param docid The name of file
	 * @param path The path of the file
	 * @param force
	 * @throws GafDbOperationsException
	 */
	public void bulkload(Reader reader, String docid, String path, boolean force) throws GafDbOperationsException{
		
		GafHibObjectsBuilder builder = new GafHibObjectsBuilder();
		
		try{
			//Read the whole document/ read partial rows. The rows limit is configured in the gold.properties file
			GafDocument doc = builder.buildDocument(reader, docid, path);
			
			ArrayList<GafDocument> docs = new ArrayList<GafDocument>();
			if(doc != null)
				docs.add(doc);

			//try to get GafDocument for the next block of rows
			doc = builder.getNextSplitDocument();
			if(doc != null)
				docs.add(doc);
			
			//checking wehther the document is splitted
			boolean split = docs.size()>1;
			
			//if the document is splitted then data will be populated in the temporary tables (the tables with prefix)
			//, and create the temporary tables overriding the existing temporary tables.
			boolean _force = split || force;
			
			while(!docs.isEmpty()){
				doc = docs.remove(0);
				
				//if split bulkload in the temporary tables otherwise bulkload in the main database tables
				bulkLoad(doc, split ? GoConfigManager.getInstance().getGoldDetlaTablePrefix() : "", _force, split);
				
				//we only want to override the tables in the first iteration
				if(_force)
					_force = false;
				
				//try to get the document from the next block of rows
				doc = builder.getNextSplitDocument();
				if(doc != null)
					docs.add(doc);
				
			}

			LOG.info("Commiting Bulk Load.");
			
			if(split){
				//merge changes in the main database tables
				
				GafObjectsFactory factory = new GafObjectsFactory();
				List docsList = factory.getGafDocument();

				//merge rows through insert into select statements
				BulkloadWork splitUpdateWork = new BulkloadWork(GoConfigManager.getInstance().getGoldDetlaTablePrefix(), docsList.isEmpty());
				Session session = factory.getSession();
				session.doWork(splitUpdateWork);

				session.getTransaction().commit();
				
			}
			
			//save the last update date of the document
			DbOperations.saveChangesHistory(docid);
			
			LOG.info("Bulk load is commited");
			
			
		}catch(Exception ex){
			throw new GafDbOperationsException(ex);
		}
	}
	
	
	/**
	 * This method bulk-loads the gafDocument in the gaf tables having the prefix (provided in the input).
	 */
	private void bulkLoad(GafDocument gafDocument, String prefix, boolean force, boolean split) throws GafDbOperationsException{
		if(DEBUG)
			LOG.debug("--");
		
		if(prefix == null)
			prefix = "";
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}

		//try to override the existing schema
		if(force){
			buildSchema(true, prefix);
		}
		
		GafObjectsFactory	factory = new GafObjectsFactory();

		List<String> tablesNames = new ArrayList<String>();

		Session session = null;
		
		if(split || prefix.length()>0){
			//if split or prefix is provided then all sql statements (select, insert and delete) 
			//executed from the session object will be intercepted and, the statements will be rebuild
			//by replacing the tables name with prefix + table name.
			session =GoldObjectFactory.buildDeltaObjectFactory().getSession();
		}else
			session = factory.getSession();
		
		tablesNames.add("gene_annotation");
		
		//If the gaf file is splitted then the bioentities could be share between between splitted documents. 
		//In this case bulk load (copy command) will not work. The bioentities will be stored by sesssion.saveOrUpdate() method 
		//If split then don't bulkload the bioentity table.
		if(!split){
			tablesNames.add("bioentity");
		}
		
		//with_info, composite_qualifier, extension_expression and bioentity (if split) tables are populated with session.saveOrUpdate command
		bulkLoadHibernate(session, gafDocument, split);
			
		//dump gene_annotation and bioentity (if not split) into tab delimited files
		List<String> list = dumpFiles(prefix, gafDocument, tablesNames);
		
		//load the tab delimited files into database via bulkload (copy command)
		loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), list);

		//update gaf_document table
		session.saveOrUpdate(gafDocument);

		//commit the all changes
		session.getTransaction().commit();
		
		LOG.info("Bulk Load completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
		}
		
	}
	
	
	/**
	 * The {@link org.geneontology.gaf.hibernate.WithInfo}, {@link org.geneontology.gaf.hibernate.CompositeQualifier} and {@link org.geneontology.gaf.hibernate.ExtensionExpression} saved 
	 * in this method can be duplicate with the existing data in the database.
	 * That's why they are loaded through hibernate which uses internally insert or update statements.  
	 * @param session
	 * @param gafDocument
	 * @param split
	 */
	private void bulkLoadHibernate(Session session, GafDocument gafDocument, boolean split){

		
		for(String id: gafDocument.getWithInfosIds()){
			List<WithInfo> list = gafDocument.getWithInfos(id);
			for(WithInfo wi: list){
				session.saveOrUpdate(wi);
			}
		}

		
		for(String id: gafDocument.getCompositeQualifiersIds()){
			for(CompositeQualifier cq: gafDocument.getCompositeQualifiers(id)){
				session.saveOrUpdate(cq);
			}
		}
		
		for(String id: gafDocument.getExtensionExpressionIds()){
			for(ExtensionExpression ex: gafDocument.getExpressions(id)){
				session.saveOrUpdate(ex);
			}
		}
		
		if(split){
			for(Bioentity entity: gafDocument.getBioentities()){
				session.saveOrUpdate(entity);
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
	/*public List<String> dumpFiles(String tablePrefix, String gafFile) throws GafDbOperationsException{
		
		try{
			gafDocument = buildGafDocument(gafFile);
		}catch(IOException ex){
			throw new GafDbOperationsException("An Error occured while building GAF document", ex);
		}
			

		return dumpFiles(tablePrefix, gafDocument);
	}

	public List<String> dumpFiles(String tablePrefix, GafDocument gafDocument) throws GafDbOperationsException{
		return dumpFiles(tablePrefix, gafDocument, new ArrayList<String>());
	}*/

		
	
	public List<String> dumpFiles(String tablePrefix, GafDocument gafDocument, List<String> tables) throws GafDbOperationsException{
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
			list= loader.loadAll(tables);
		}catch(IOException ex){
			throw new GafDbOperationsException(ex);
		}
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;
		
	}
	
	/*public GafDocument buildGafDocument(Reader reader, String docId, String path) throws IOException{
		for(DbOperationsListener listener: listeners){
			listener.startDomLoad();
		}

		GafHibObjectsBuilder builder = new GafHibObjectsBuilder();
		
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
	}*/
	
	
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

		//if path is relative the function will convert it to URI
		gafLocation = toURI(gafLocation);
		GafURLFetch fetch = new GafURLFetch(gafLocation);
		Reader reader = new InputStreamReader((InputStream)fetch.next());
		
		update(reader, fetch.getCurrentGafFile(), fetch.getCurrentGafFilePath());
		
	}

	
	/**
	 * 
	 * @param reader
	 * @param docid name of the gaf
	 * @param path
	 * @throws GafDbOperationsException
	 */
	public void update(Reader reader, String docid, String path) throws GafDbOperationsException{
		GafHibObjectsBuilder builder = new GafHibObjectsBuilder();
		
		try{
			//build the document by reading a fixed block of rows. The rows limit is set in the gold.properties file
			GafDocument doc = builder.buildDocument(reader, docid, path);
			ArrayList<GafDocument> docs = new ArrayList<GafDocument>();
			if(doc != null)
				docs.add(doc);
			
			//try to read the next block of rows
			doc = builder.getNextSplitDocument();
			if(doc != null)
				docs.add(doc);
			
			//check the gaf file is splitted
			boolean split = docs.size()>1;
			boolean _force = true;
			int docsNumber = docs.size();
			while(!docs.isEmpty()){
				doc = docs.remove(0);
		
				//perform bulkload into the temporary tables (the tables with a prefix). If the iteration
				//is first time the create schema first
				bulkLoad(doc, GoConfigManager.getInstance().getGoldDetlaTablePrefix(), _force, split);
				
				//we only the schema creation in the first iteration of the loop
				if(_force)
					_force = false;
				
				doc = builder.getNextSplitDocument();
				if(doc != null)
					docs.add(doc);
				
			}

			LOG.info("Commiting Update.");

			//if atleast one document is build above the merge the changes
			if(docsNumber>0){

				GafObjectsFactory factory = new GafObjectsFactory();

				UpdateWork updateWork = new UpdateWork(GoConfigManager.getInstance().getGoldDetlaTablePrefix(), docid);
				Session session = factory.getSession();
				session.doWork(updateWork);

				for(Object obj: updateWork.objectsToBeUpdated){
					session.saveOrUpdate(obj);
				}
				
				DbOperations.saveChangesHistory(docid);
				
				if(session.isOpen())
					session.getTransaction().commit();
			}
				
			LOG.info("Update is commited");
			
			
		}catch(Exception ex){
			throw new GafDbOperationsException(ex);
		}
	}
	
	
	public void addDbOperationsListener(DbOperationsListener listener){
		if(!listeners.contains(listener))
			listeners.add(listener);
	}

	public void removeDbOperationsListener(DbOperationsListener listener){
		listeners.remove(listener);
	}
	
	/**
	 * This work is executed through hibernate session object. If the gaf file large enough then it is loaded through split
	 * strategy, and this work is only executed when split methodology is used on a gaf file.
	 * It merges data from temporary tables to main database tables via insert into select command.
	 * @author shahidmanzoor
	 *
	 */
	private class BulkloadWork implements Work{

		private String prefix;
		
		private boolean isFirstTimeBulkload;
		
		BulkloadWork(String prefix, boolean isFirstTimeBulkload){
			this.prefix = prefix;
			this.isFirstTimeBulkload = isFirstTimeBulkload;
		}
		
		@Override
		public void execute(Connection connection) throws SQLException {
			Statement stmt= connection.createStatement();
			

			//merge data into gene_annotation table
			stmt.executeUpdate("insert into gene_annotation (bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document) select bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document from " + prefix + "gene_annotation");
			//merge data into bioentity

			stmt = connection.createStatement();
			stmt.executeUpdate("insert into bioentity (id, symbol, full_name, type_cls,taxon_cls, db, gaf_document) select id, symbol, full_name, type_cls,taxon_cls, db, gaf_document from " +prefix+ "bioentity");
			
			if(isFirstTimeBulkload){
				//if database is empty then this block is executed
			
				//merge data into composite_qualifier
				stmt = connection.createStatement();
				stmt.executeUpdate("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier");
			
				//merge data into with_info
				stmt = connection.createStatement();
				stmt.executeUpdate("insert into with_info (id, with_xref) select id, with_xref from " + prefix + "with_info");
				
				//merge data into extension_expression
				stmt = connection.createStatement();
				stmt.execute("insert into extension_expression (id, relation, cls) select id, relation, cls from " + prefix + "extension_expression");
			}else{
				//this block is executed when the database has already some data.
				
				//for each insert statement below: first compute difference via EXCEPT operator between main database tables and the tables with prefix and then insert 
				//the difference into the main tables
				
				stmt = connection.createStatement();
				stmt.execute("insert into with_info (id, with_xref) select id, with_xref from " + prefix + "with_info EXCEPT select id, with_xref from with_info");
				
				stmt = connection.createStatement();
				stmt.execute("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier except select id, qualifier_obj from composite_qualifier");

				stmt = connection.createStatement();
				stmt.execute("insert into extension_expression (id, relation, cls) select id, relation, cls from " + prefix + "extension_expression except select id, relation, cls from extension_expression");

			}
			
			
		}
		
	}
	
	/**
	 * This work is executed during the processing of the update command.
	 * @author shahidmanzoor
	 *
	 */
	private class UpdateWork implements Work{

		private String prefix;
		
		//gaf file name
		private String docid;

		/**
		 * The collection holds the objects which represents new changes for the database. 
		 */
		private List objectsToBeUpdated;
		
		private UpdateWork(String prefix, String docId){
			this.prefix = prefix;
			this.docid = docId;
			this.objectsToBeUpdated = new ArrayList();
			
		}
		
		@Override
		public void execute(Connection connection) throws SQLException {

			//compute changes for bioentity table
			Statement stmt= connection.createStatement();
			ResultSet rs = stmt.executeQuery("select * from "+ prefix + "bioentity EXCEPT select * from bioentity where gaf_document='"+docid + "'");
			while(rs.next()){
				org.geneontology.gaf.hibernate.Bioentity entity = new org.geneontology.gaf.hibernate.Bioentity(
						rs.getString("id"), rs.getString("symbol"), rs.getString("full_name"), 
						rs.getString("type_cls"), rs.getString("taxon_cls"), rs.getString("db"), rs.getString("gaf_document"));
				this.objectsToBeUpdated.add(entity);
			}

			//compute changes for gaf_document table
			stmt = connection.createStatement();
			rs = stmt.executeQuery("select * from "+ prefix + "gaf_document EXCEPT select * from gaf_document where id='"+docid + "'");
			while(rs.next()){
				org.geneontology.gaf.hibernate.GafDocument doc = new org.geneontology.gaf.hibernate.GafDocument(rs.getString("id"),rs.getString("document_path"));
				this.objectsToBeUpdated.add(doc);
			}
			
			
			//delete 
			stmt = connection.createStatement();
			rs = stmt.executeQuery("select * from bioentity where gaf_document='"+docid + "' EXCEPT select * from "+ prefix + "bioentity");
			
			while(rs.next()){
				stmt = connection.createStatement();
				stmt.execute("delete from bioentity where id='"+rs.getString("id")+"'");
			}

			stmt = connection.createStatement();
			rs = stmt.executeQuery("select * from "+ prefix + "gene_annotation EXCEPT select * from gene_annotation where gaf_document='"+docid+"'");
			
			while(rs.next()){
				org.geneontology.gaf.hibernate.GeneAnnotation ga = new org.geneontology.gaf.hibernate.GeneAnnotation(
						rs.getString("bioentity"), rs.getBoolean("is_contributes_to") , rs.getBoolean("is_integral_to") 
						, rs.getString("composite_qualifier") , rs.getString("cls"), 
						rs.getString("reference_id") , rs.getString("evidence_cls") ,rs.getString("with_expression")  , 
						rs.getString("acts_on_taxon_id") , rs.getString("last_update_date") , rs.getString("assigned_by"),
						rs.getString("extension_expression") , rs.getString("gene_product_form") , 
						rs.getString("gaf_document"));
				this.objectsToBeUpdated.add(ga);
			}
			
			
			stmt = connection.createStatement();
			String q= "select * from gene_annotation where gaf_document='"+docid+"' EXCEPT select * from "+ prefix + "gene_annotation ";
			rs = stmt.executeQuery(q);

			while(rs.next()){
				String sql = "delete from gene_annotation where bioentity='"+rs.getString("bioentity")+
						"' and gaf_document ='"+rs.getString("gaf_document")+ "' and composite_qualifier='" + rs.getString("composite_qualifier")
						+ "' and cls='"+rs.getString("cls") + "' and reference_id='"+ rs.getString("reference_id") + "' and " +
								"evidence_cls='"+rs.getString("evidence_cls") + "'";
				stmt = connection.createStatement();
				stmt.execute(sql);
			}
			
			
			stmt.execute("insert into with_info (id, with_xref) select id, with_xref from " + prefix + "with_info EXCEPT select id, with_xref from with_info");
			stmt.execute("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier except select id, qualifier_obj from composite_qualifier");
			stmt.execute("insert into extension_expression (id, relation, cls) select id, relation, cls from " + prefix + "extension_expression except select id, relation, cls from extension_expression");
			
		}
		
	}
	
}
