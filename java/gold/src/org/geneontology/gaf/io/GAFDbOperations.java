package org.geneontology.gaf.io;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.zip.GZIPInputStream;
import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafHibObjectsBuilder;
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.hibernate.Session;
import org.hibernate.jdbc.Work;
import owltools.gaf.Bioentity;
import owltools.gaf.CompositeQualifier;
import owltools.gaf.ExtensionExpression;
import owltools.gaf.GeneAnnotation;
import owltools.gaf.WithInfo;

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
	
	public void bulkload(Reader reader, String docid, String path, boolean force) throws GafDbOperationsException{
		GafHibObjectsBuilder builder = new GafHibObjectsBuilder();
		
		try{
			GafDocument doc = builder.buildDocument(reader, docid, path);
			ArrayList<GafDocument> docs = new ArrayList<GafDocument>();
			if(doc != null)
				docs.add(doc);
			
			doc = builder.getNextSplitDocument();
			if(doc != null)
				docs.add(doc);
			
			boolean split = docs.size()>1;
			boolean _force = split || force;
			
			while(!docs.isEmpty()){
				doc = docs.remove(0);
				
				
				bulkLoad(doc, split ? GoConfigManager.getInstance().getGoldDetlaTablePrefix() : "", _force, split);
				
				if(_force)
					_force = false;
				
				doc = builder.getNextSplitDocument();
				if(doc != null)
					docs.add(doc);
				
			}

			LOG.info("Commiting Bulk Load.");
			if(split){
				GafObjectsFactory factory = new GafObjectsFactory();
				List docsList = factory.getGafDocument();

				BulkloadWork splitUpdateWork = new BulkloadWork(GoConfigManager.getInstance().getGoldDetlaTablePrefix(), docsList.isEmpty());
				Session session = factory.getSession();
				session.doWork(splitUpdateWork);

				session.getTransaction().commit();
				
			}
			
			DbOperations.saveChangesHistory(docid);
			
			LOG.info("Bulk load is commited");
			
			
		}catch(Exception ex){
			throw new GafDbOperationsException(ex);
		}
	}
	
	public void bulkLoad(GafDocument gafDocument, boolean force) throws GafDbOperationsException{
		/*if(DEBUG)
			LOG.debug("--");
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
		
		
		List<String> list = dumpFiles("", gafDocument);
		
		loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), list);

		LOG.info("Bulk Load completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
		}*/
		
		bulkLoad(gafDocument, "", force);
		
	}
	
	public void bulkLoad(GafDocument gafDocument, String prefix, boolean force) throws GafDbOperationsException{
		bulkLoad(gafDocument, prefix, force, false);
	}
	
	//private GafObjectsFactory factory;
	//private Session session;
	
	private void bulkLoad(GafDocument gafDocument, String prefix, boolean force, boolean split) throws GafDbOperationsException{
		if(DEBUG)
			LOG.debug("--");
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}

		if(force){
			buildSchema(true, prefix);
		}
		
		//if(factory == null || !split || gafDocument == null){
		GafObjectsFactory	factory = new GafObjectsFactory();
		//}

		List gafDocs = factory.getGafDocument();
		
		List<String> tablesNames = new ArrayList<String>();

		Session session = null;
		
		if(split || prefix.length()>0){
			session =GoldObjectFactory.buildDeltaObjectFactory().getSession();
		}else
			session = factory.getSession();
		
		if(!gafDocs.isEmpty() || split){
			tablesNames.add("gene_annotation");
			if(!split){
				tablesNames.add("bioentity");
			}
			
			bulkLoadHibernate(session, gafDocument, split);
			
		}
		
		List<String> list = dumpFiles(prefix, gafDocument, tablesNames);
		
		loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), list);

		session.saveOrUpdate(gafDocument);

		session.getTransaction().commit();
		
		LOG.info("Bulk Load completed successfully");

		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
		}
		
	}
	
	
	
	private void bulkLoadHibernate(Session session, GafDocument gafDocument, boolean split){

		
	//	session.save(gafDocument);
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
		return dumpFiles(tablePrefix, gafDocument, new ArrayList<String>());
	}

		
	
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
	
	public GafDocument buildGafDocument(Reader reader, String docId, String path) throws IOException{
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
	/*public void loadTsvFiles(String tsvFilesDir, List<String> list) throws GafDbOperationsException{
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
		
		loadTsvFiles(tsvFilesDir, list);
		
	}*/

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
	private boolean isFirstUpdateIteration;
	public void update(GafDocument gafDocument, boolean splitt) throws GafDbOperationsException{
		if(LOG.isDebugEnabled()){
			LOG.debug("-");
		}
		
		this.isFirstUpdateIteration = false;
		
		
		if(gafDocument != null){
		
			GafObjectsFactory f = new GafObjectsFactory();
			
			this.gafDocument = gafDocument;
			if(f.getGafDocument().isEmpty()){
				bulkLoad(gafDocument, false);
				return;
			}
			
			for(DbOperationsListener listener: listeners){
				listener.updateStart();
			}
			
			GoConfigManager manager = GoConfigManager.getInstance();

			if(!splitt || (splitt && !isSchemaCreted)){
				
				buildSchema(true, manager.getGoldDetlaTablePrefix());
				buildSchema(true, "tmp-"+manager.getGoldDetlaTablePrefix());
				isSchemaCreted = true;
				isFirstUpdateIteration = true;

			}

			
			List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), gafDocument);
			
			
			List<String> ll = new ArrayList<String>();
			
			ll.add(manager.getGoldDetlaTablePrefix()+"gene_annotation");
			
			loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), ll);
			isSchemaCreted = true;
			
			LOG.info("updating bioentity table.");
			
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
		
		LOG.info("last step of update.");
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
	
	/*private void saveOrUpdate(Session session, Collection objects){
		
		for(Object obj: objects){
			session.saveOrUpdate(obj);
		}
	}*/
	
	
	public void update(Reader reader, String docid, String path) throws GafDbOperationsException{
		GafHibObjectsBuilder builder = new GafHibObjectsBuilder();
		
		try{
			GafDocument doc = builder.buildDocument(reader, docid, path);
			ArrayList<GafDocument> docs = new ArrayList<GafDocument>();
			if(doc != null)
				docs.add(doc);
			
			doc = builder.getNextSplitDocument();
			if(doc != null)
				docs.add(doc);
			
			boolean split = docs.size()>1;
			boolean _force = true;
			int docsNumber = docs.size();
			while(!docs.isEmpty()){
				doc = docs.remove(0);
				
				bulkLoad(doc, GoConfigManager.getInstance().getGoldDetlaTablePrefix(), _force, split);
				
				if(_force)
					_force = false;
				
				doc = builder.getNextSplitDocument();
				if(doc != null)
					docs.add(doc);
				
			}

			LOG.info("Commiting Update.");
			if(docsNumber>0){
				GafObjectsFactory factory = new GafObjectsFactory();
//				List docsList = factory.getGafDocument();

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
			stmt.executeUpdate("insert into gene_annotation (bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document) select bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document from " + prefix + "gene_annotation");

			
//			stmt= connection.createStatement();
			stmt.executeUpdate("insert into bioentity (id, symbol, full_name, type_cls,taxon_cls, db, gaf_document) select id, symbol, full_name, type_cls,taxon_cls, db, gaf_document from " +prefix+ "bioentity");
			
			if(isFirstTimeBulkload){
			
	//			stmt = connection.createStatement();
				stmt.executeUpdate("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier");
			
		//		stmt = connection.createStatement();
				stmt.executeUpdate("insert into with_info (id, with_xref) select id, with_xref from " + prefix + "with_info");
				
			//	stmt = connection.createStatement();
				stmt.execute("insert into extension_expression (id, relation, cls) select id, relation, cls from " + prefix + "extension_expression");
			}else{
				//stmt.execute("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier")
				/*ResultSet rs= stmt.executeQuery("select t2.* from with_info t1, " + prefix+"with_info t2 where t1.id = t2.id");
				while(rs.next()){
					objectsToBeUpdated.add(new org.geneontology.gaf.hibernate.WithInfo(rs.getString("id"), rs.getString("with_xref")));
				}*/
				
				stmt.execute("insert into with_info (id, with_xref) select id, with_xref from " + prefix + "with_info EXCEPT select id, with_xref from with_info");
				
				stmt.execute("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier except select id, qualifier_obj from composite_qualifier");
				
				stmt.execute("insert into extension_expression (id, relation, cls) select id, relation, cls from " + prefix + "extension_expression except select id, relation, cls from extension_expression");

			}
			
			
		}
		
	}
	
	private class UpdateWork implements Work{

		private String prefix;
		private String docid;
		private List objectsToBeUpdated;
		
		private UpdateWork(String prefix, String docId){
			this.prefix = prefix;
			this.docid = docId;
			this.objectsToBeUpdated = new ArrayList();
			
		}
		
		@Override
		public void execute(Connection connection) throws SQLException {
			Statement stmt= connection.createStatement();

			/*try{
				stmt.execute("drop table "+prefix+"tmpgene_annotation");
			}catch(Exception ex){
				//ignore this
			}

			try{
				stmt.execute("drop table "+prefix+"tmpbioentity");
			}catch(Exception ex){
				//ignore this
			}
			
			
			
			stmt.execute("create table abc (id varchar)");
			
			stmt.execute("create table "+prefix+"tmpgene_annotation  (bioentity VARCHAR NOT NULL, " +
				"composite_qualifier VARCHAR, is_contributes_to BOOLEAN, is_integral_to BOOLEAN, " +
				"cls VARCHAR NOT NULL, reference_id VARCHAR, evidence_cls VARCHAR, with_expression VARCHAR, " +
				"acts_on_taxon_id VARCHAR, last_update_date VARCHAR, assigned_by VARCHAR, extension_expression VARCHAR, " +
				"gene_product_form VARCHAR, gaf_document VARCHAR)");
			
			
			stmt.execute("create table "+prefix+"tmpbioentity" +
					  "id VARCHAR PRIMARY KEY, symbol VARCHAR NOT NULL, full_name VARCHAR NOT NULL," + 
					  "type_cls VARCHAR NOT NULL, taxon_cls VARCHAR NOT NULL, db VARCHAR, gaf_document varchar)");*/
			
		
			stmt = connection.createStatement();
			ResultSet rs = stmt.executeQuery("select * from "+ prefix + "bioentity EXCEPT select * from bioentity where gaf_document='"+docid + "'");
			while(rs.next()){
				org.geneontology.gaf.hibernate.Bioentity entity = new org.geneontology.gaf.hibernate.Bioentity(
						rs.getString("id"), rs.getString("symbol"), rs.getString("full_name"), 
						rs.getString("type_cls"), rs.getString("taxon_cls"), rs.getString("db"), rs.getString("gaf_document"));
				this.objectsToBeUpdated.add(entity);
			}

			stmt = connection.createStatement();
			rs = stmt.executeQuery("select * from "+ prefix + "gaf_document EXCEPT select * from gaf_document where id='"+docid + "'");
			while(rs.next()){
				org.geneontology.gaf.hibernate.GafDocument doc = new org.geneontology.gaf.hibernate.GafDocument(rs.getString("id"),rs.getString("document_path"));
				this.objectsToBeUpdated.add(doc);
			}
			
			
			stmt = connection.createStatement();
			rs = stmt.executeQuery("select * from bioentity where gaf_document='"+docid + "' EXCEPT select * from "+ prefix + "bioentity");
			
			while(rs.next()){
				stmt = connection.createStatement();
				stmt.execute("delete from bioentity where id='"+rs.getString("id"));
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
			
			
			/*stmt.execute("insert into "+prefix+"tmpbioentity (id, symbol, full_name, type_cls,taxon_cls, db, gaf_document) select id, symbol, full_name, type_cls,taxon_cls, db, gaf_document from bioentity where gaf_document='"+this.docid+"'");
			
			ResultSet rs = stmt.executeQuery("select * from "+ prefix + "tmpbioentity EXCEPT select * from "+prefix + "bioentity");
			while(rs.next()){
				stmt.execute("delete from bioentity where id='"+rs.getString("id")+"'");
				stmt.execute("delete from " +prefix+ "tmpbioentity where id='"+rs.getString("id")+"'");

			}
			
			stmt.execute("insert into " +prefix+ "tmpgene_annotation (bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document) select bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document from gene_annotation where gaf_document='"+this.docid+"'");
			
			rs = stmt.executeQuery("select * from "+ prefix + "tmpgene_annotation EXCEPT select * from "+prefix + "gene_annotation");
			while(rs.next()){
				stmt.execute("delete from gene_annotation where bioentity='"+rs.getString("bioentity")+
						"' and gaf_document ='"+rs.getString("gaf_document")+ "' and composite_qualifier='" + rs.getString("composite_qualifier")
						+ " and cls='"+rs.getString("cls") + "' and reference_id='"+ rs.getString("reference_id") + "' and " +
								"evidence_cls='"+rs.getString("evidence_cls") + "' and ");
								
			}
			
			stmt.execute("insert into bioentity select * from "+prefix + "bioentity EXCEPT select * from "+ prefix +"tmpbioentity");

			stmt.execute("insert into gene_annotation select * from "+prefix + "gene_annotation EXCEPT select * from "+ prefix +"tmpgene_annotation");*

			/*
			ResultSet rs= stmt.executeQuery("select * from " + prefix + "bioentity EXCEPT select * from bioentity");
			while(rs.next()){
				org.geneontology.gaf.hibernate.Bioentity entity = new org.geneontology.gaf.hibernate.Bioentity(
						rs.getString("id"), rs.getString("symbol"), rs.getString("full_name"), 
						rs.getString("type_cls"), rs.getString("taxon_cls"), rs.getString("db"), rs.getString("gaf_document"));
				this.objectsToBeUpdated.add(entity);
			}
			
			
			rs = stmt.executeQuery("select * from "+ prefix + "gene_annotation EXCEPT select * from gene_annotation");
			while(rs.next()){
				org.geneontology.gaf.hibernate.GeneAnnotation ga = new org.geneontology.gaf.hibernate.GeneAnnotation(
						rs.getString("bioentity"), rs.getBoolean("is_contributes_to") , rs.getBoolean("is_integral_to") 
						, rs.getString("composite_qualifier") , rs.getString("cls"), 
						rs.getString("reference_id") , rs.getString("evidence_cls") ,rs.getString("with_expression")  , 
						rs.getString("acts_on_taxon_id") , rs.getString("last_update_date") , rs.getString("assigned_by"),
						rs.getString("extension_expression") , rs.getString("gene_product_form") , 
						rs.getString("gafDocument"));
				this.objectsToBeUpdated.add(ga);
			}
			*/
//			stmt.execut
	//		stmt.executeQ
			
	//		stmt.execute("insert into bioentity (id, symbol, full_name, type_cls,taxon_cls, db, gaf_document) select * from "+prefix+"bioentity EXCEPT SELECT * from "+prefix+"tmpbioentity");
	//		stmt.execute("insert into gene_annotation (bioentity, composite_qualifier, is_contributes_to, is_integral_to, cls, reference_id, evidence_cls, with_expression, acts_on_taxon_id, last_update_date, assigned_by, extension_expression, gene_product_form, gaf_document) select * from "+prefix+"gene_annotation EXCEPT select * from " + prefix +"tmpgene_annotation");
			
			
			stmt.execute("insert into with_info (id, with_xref) select id, with_xref from " + prefix + "with_info EXCEPT select id, with_xref from with_info");
			stmt.execute("insert into composite_qualifier (id, qualifier_obj) select id, qualifier_obj from " + prefix + "composite_qualifier except select id, qualifier_obj from composite_qualifier");
			stmt.execute("insert into extension_expression (id, relation, cls) select id, relation, cls from " + prefix + "extension_expression except select id, relation, cls from extension_expression");
			
		}
		
	}
	
}
