package org.geneontology.gold.io;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gold.hibernate.model.AllOnlyRelationship;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.AnnotationAssertion;
import org.geneontology.gold.hibernate.model.AnnotationProperty;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ClsIntersectionOf;
import org.geneontology.gold.hibernate.model.ClsUnionOf;
import org.geneontology.gold.hibernate.model.DatabaseChangesHistory;
import org.geneontology.gold.hibernate.model.DisjointWith;
import org.geneontology.gold.hibernate.model.EquivalentTo;
import org.geneontology.gold.hibernate.model.GoldDeltaFactory;
import org.geneontology.gold.hibernate.model.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.InferredAllSomeRelationship;
import org.geneontology.gold.hibernate.model.InferredSubclassOf;
import org.geneontology.gold.hibernate.model.NeverSomeRelationship;
import org.geneontology.gold.hibernate.model.ObjAlternateId;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.ObjDefinitionXref;
import org.geneontology.gold.hibernate.model.ObjSubset;
import org.geneontology.gold.hibernate.model.ObjXref;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.hibernate.model.OntologyAlternateLabelType;
import org.geneontology.gold.hibernate.model.OntologyAnnotation;
import org.geneontology.gold.hibernate.model.OntologyImports;
import org.geneontology.gold.hibernate.model.OntologySubset;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.RelationChain;
import org.geneontology.gold.hibernate.model.RelationDisjointWith;
import org.geneontology.gold.hibernate.model.RelationEquivalenTo;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.hibernate.model.SubrelationOf;
import org.geneontology.gold.io.postgres.PostgresDialect;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.hibernate.Session;
import org.hibernate.jdbc.Work;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import owltools.graph.OWLGraphWrapper;

/**
 * This class provides adminstrative operations for the GOLD database. The two major operations of this class are update and bulkload
 * The bulkload operation first generate tab delimited files for the database tables and then load the files into the database through its
 * native command. The update process first perform the bulkload into temporary tables (tables names are prefixed with the value of geneontology.gold.deltatableprefix property).
 * Then the update command computes changes and the changes are saved into the databse through the hibernate layer.  
 * @author Shahid Manzoor
 *
 */
public class DbOperations implements DbOperationsInterface{

	/**
	 * Only one operation either update or bulkload can run. 
	 * This variable keeps track if any of these operation running and
	 * throw exception if another operation is called while currently an operation
	 * is in progress.
	 */
	private static boolean isOperationRunning  = false;
	
	private List<DbOperationsListener> listeners;
	
	private static Logger LOG = Logger.getLogger(DbOperations.class);
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	public DbOperations(){
		listeners = new ArrayList<DbOperationsListener>();
	}
	
	public static boolean IsOperationRunning(){
		return isOperationRunning;
	}
	
	/**
	 * Loads the contents of the obo file whose path is supplied 
	 * the geneontology.gold.obofil property.
	 * @param force
	 * @throws Exception
	 */
	public void bulkLoad(boolean force) throws Exception{
		if(DEBUG){
			LOG.debug("-");
		}
		List files = GoConfigManager.getInstance().getDefaultOntologyLocations();
		
		if(files == null || files.size()==0){
			throw new Exception("Ontology File Location is not Found specified in the geneontology.gold.ontologylocation property" );
		}
		
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
	public void bulkLoad(String ontologyLocation, boolean force) throws Exception{
		if(DEBUG)
			LOG.debug(ontologyLocation);
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
		
		
		_bulkLoad(buildOWLGraphWrapper(ontologyLocation), force);
	}
	

	/**
	 * This method saves the last update date of ontology
	 * @param ontologId: Refer to ontology id
	 */
	public static void saveChangesHistory(String ontologId){
		DatabaseChangesHistory changeHistory = new DatabaseChangesHistory(ontologId, Calendar.getInstance().getTime());
		
		GoldObjectFactory factory =  GoldObjectFactory.buildDefaultFactory();
		Session session= factory.getSession();
		session.save(changeHistory);
		session.getTransaction().commit();
		
	}


	/**
	 * Perform bulkload on the ontology referenced by wrapper (instance of {@link OWLGraphWrapper}).
	 * The bulkload operation first generate tab delimited files for the database tables and then load the files into the database through its
	 * native command.	 * @param wrapper
	 * @param force
	 * @throws Exception
	 */
	private void _bulkLoad(OWLGraphWrapper wrapper, boolean force) throws Exception{

		if(DEBUG){
			LOG.debug("Bulk Load for: " + wrapper.getOntologyId());
		}
		
		//only one database update operation is allowed 
		if(isOperationRunning){
			throw new RuntimeException("Another operating is still running. ");
		}
		
		try{
			isOperationRunning = true;
	
			//generates tab delimited files for the database tables
			List<String> list = dumpFiles("", wrapper);
			
			//load the tab delimited files into the database.
			loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), list);
		
			//save the last update date of the onotlogy
			saveChangesHistory(wrapper.getOntologyId());
			
			LOG.info("Bulk Load completed successfully");
	
		}catch(Exception ex){
			throw ex;
		}finally{
			isOperationRunning = false;
		}
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadEnd();
		}
		
	}
	
	/**
	 * 
	 * @param wrapper
	 * @param force
	 * @throws Exception
	 */
	public void bulkLoad(OWLGraphWrapper wrapper, boolean force) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}

		_bulkLoad(wrapper, force);
	 
	}
	
	
	/**
	 * This method dumps the obo ontology file (oboFile) as tab separated files
	 * for GOLD database to be used for bulk loading 
	 * @param tablePrefix  The prefix is used when the tables are dumped for the temporary tables during the update process.
	 * @param oboFile
	 * @return It returns the name of the tables for which the files are dumped
	 * @throws Exception
	 */
	public List<String> dumpFiles(String tablePrefix, String oboFile) throws Exception{
		return dumpFiles(tablePrefix, buildOWLGraphWrapper(oboFile));
	}
	
	/**
	 * This method dumps the owl ontology as tab separated files
	 * for GOLD database to be used for bulk loading 
	 * @param tablePrefix  The prefix is used when the tables are dumped for the temporary tables during the update process.
	 * @param wrapper
	 * @return
	 * @throws Exception
	 */
	public List<String> dumpFiles(String tablePrefix, OWLGraphWrapper wrapper) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(DEBUG){
			LOG.debug("-");
		}
		
		
		GoConfigManager manager = GoConfigManager.getInstance();

		OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, manager.getTsvFilesDir(), tablePrefix);
		
		List<String> list = loader.dumpBulkLoadTables();
		
		LOG.info("Tables dump completed");
		
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesEnd();
		}
		
		return list;
		
	}
	
	
	/**
	 * 
	 * @return This method builds array of the instances of the {@link OWLGraphWrapper} class; wrapper around the OWLOntology.
	 * The graph wrappers are built from the default location ontology configured at conf/gold.properties file.
	 * @throws IOException 
	 * @throws OWLOntologyCreationException 
	 */
	public OWLGraphWrapper[] buildOWLGraphWrappers() throws IOException, OWLOntologyCreationException{
		
		List  list = GoConfigManager.getInstance().getDefaultOntologyLocations();
		
		OWLGraphWrapper graph[] = new OWLGraphWrapper[list.size()];
		
		for(int i=0;i<list.size();i++){
			graph[i] = buildOWLGraphWrapper(list.get(i).toString());
		}
	
		
		return graph;
	
	}
	
	public OWLGraphWrapper buildOWLGraphWrapper(String ontologyLocation) throws IOException, OWLOntologyCreationException{
		OWLOntology ontology = buildOWLOntology(ontologyLocation);
		
		OWLGraphWrapper wrapper = new OWLGraphWrapper(ontology);

		return wrapper;
	}	
	

	public OWLOntology buildOWLOntology(String ontologyLocation) throws IOException, OWLOntologyCreationException{
		if(ontologyLocation == null)
			throw new FileNotFoundException("The file is not found");
		
		LOG.info("Loading Ontology file....."+ ontologyLocation);

		for(DbOperationsListener listener: listeners){
			listener.startDomLoad();
		}
		
		OWLOntology ontology = null;
		if(ontologyLocation.endsWith(".obo")){
			OBOFormatParser parser = new OBOFormatParser();
			
			OBODoc doc = parser.parse(ontologyLocation);
			
			Obo2Owl obo2owl = new Obo2Owl();
			
			ontology = obo2owl.convert(doc);
		}else{
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			if(ontologyLocation.startsWith("http:"))
				ontology = manager.loadOntologyFromOntologyDocument(IRI.create(ontologyLocation));
			else
				ontology = manager.loadOntologyFromOntologyDocument(new File(ontologyLocation));
				
		}
		
		for(DbOperationsListener listener: listeners){
			listener.endDomLoad(ontology);
		}
		
		return ontology;
	}	
	
	
	
	/**
	 * This method creates schema of GOLD database.
	 * @param force: The true value of this parameter drop of all existing tables and
	 * 			creates new ones.
	 * @param tablePrefix This prefix is used as prefix of each table created in the database.
	 * @throws Exception
	 */
	public void buildSchema(boolean force, String tablePrefix) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.buildSchemaStart();
		}
		
		if(DEBUG){
			LOG.debug("-");
		}

		//The schema is created in the database through the SchemaManager class.
		SchemaManager sm = new SchemaManager();
		GoConfigManager manager = GoConfigManager.getInstance();
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
	 * 		to be loaded in the GOLD database. The extension of the tab delimited files is .txt
	 * @throws Exception
	 */
	public void loadTsvFiles(String tsvFilesDir, List<String> list) throws Exception{
	
		for(DbOperationsListener listener: listeners){
			listener.loadTsvFilesStart();
		}
	
		
		if(DEBUG){
			LOG.debug(list + " files being loaded");
		}

		GoConfigManager manager = GoConfigManager.getInstance();
	
		//This class loads the tab delimited files into the database
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
		
		if(DEBUG){
			LOG.debug("-");
		}

		File dir = new File(tsvFilesDir);

		//Get only the list of the .txt extension files
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
		if(DEBUG){
			LOG.debug("-");
		}

		//Get the list of the onotlogies file from the system configuration 
		List list = GoConfigManager.getInstance().getDefaultOntologyLocations();
		
		if(list == null || list.size()==0){
			throw new Exception("Ontology File Locations are not specified in the geneontology.gold.ontologylocation property" );
		}
		
		for(Object obj: list)
			update(obj.toString());
	}	
	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo ontology file
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void update(String oboFile) throws Exception{
		
		updateGold(buildOWLGraphWrapper(oboFile));
		
	}
	
	/**
	 * The update process first perform the bulkload into temporary tables (tables names are prefixed with the value of geneontology.gold.deltatableprefix property).
	 * Then the update command computes changes and the changes are saved into the databse through the hibernate layer.
	 * @param wrapper
	 * @throws Exception
	 */
	public void updateGold(OWLGraphWrapper wrapper) throws Exception{
		if(DEBUG){
			LOG.debug("-");
		}

		for(DbOperationsListener listener: listeners){
			listener.updateStart();
		}
	
		//only one process for changes in the database is allowed
		if(isOperationRunning){
			throw new RuntimeException("Another operating is still running. ");
		}
		
		isOperationRunning = true;
		
		try{
			GoConfigManager manager = GoConfigManager.getInstance();
			
			List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), wrapper);
			
			//build schema for the temporary tables.
			buildSchema(true, manager.getGoldDetlaTablePrefix());
	
			loadTsvFiles(GoConfigManager.getInstance().getTsvFilesDir(), list);
			
	
			if(DEBUG){
				LOG.debug("Extracting delt hibernate objects from prefixed temporary tables");
			}
			
			//build hibernate objects which represent new changes in the database.
			
			
			GoldDeltaFactory gdf = new GoldDeltaFactory();
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
			List<DisjointWith> djList = gdf.buildDisjointWith();
			List<Ontology> ontList = gdf.buildOntology();
			List<OntologyAnnotation> ontAnnotationList = gdf.buildOntologyAnnotation();
			List<OntologyImports> ontImportsList = gdf.buildOntologyImports();
			List<OntologySubset> ontSubsetList = gdf.buildOntologySubset();
			List<ObjSubset> objSubsetList = gdf.buildObjSubset();
			List<AnnotationProperty> annPropList = gdf.buildAnnotationProperty();
			List<ObjAlternateId> objAltIdList = gdf.buildObjAlternateId();
			List<AllOnlyRelationship> allOnlyList = gdf.buildAllOnlyRelationships();
			List<NeverSomeRelationship> neverSomeList = gdf.buildNeverSomeRelationship();
			List<SubrelationOf> subRelationList = gdf.buildSubrelationOf();
			List<RelationDisjointWith> relDisjointList = gdf.buildRelationDisjointWith();
			List<RelationChain> rcList = gdf.buildRelationChain();
			List<AnnotationAssertion> annAssertionList = gdf.buildAnnotationAssertion();
			List<RelationEquivalenTo> relEqList = gdf.buildRelationEquivalenTo();
			List<OntologyAlternateLabelType> oatList = gdf.buildOntologyAlternateLabelType();

			//The hibernate objects are built from temporary tables. The associated with the session links to temporary tables
			//The session is need to be closed otherwise the hibernate objects cannot be saved by the other session.
			gdf.getSession().getTransaction().commit();
			gdf.getSession().close();
			
			if(DEBUG){
				LOG.debug("Merging the the delta objects in the GOLD database");
			}
			
			
			//Build a new session points to the main tables of the database.
			GoldObjectFactory gof = GoldObjectFactory.buildDefaultFactory();
			Session session = gof.getSession();
		//	PostgresDialect db = new PostgresDialect();
		//	Connection connection = db.getConnect();
		//	connection.setAutoCommit(false);
			
			//session.
			//session.clear();
	
			//The utility deletes the removed assertions in the ontology from the databse.
			//With the help of this utility deletes becomes part of a single transaction which is performing the updates 
			DeleteUtility du = new DeleteUtility(manager.getGoldDetlaTablePrefix(), wrapper.getOntologyId());
		
			//du.execute(connection);
			//session.reconnect(connection);

			session.doWork(du);
			
			//session the hibernate objects through the session object
			
			saveList(session, clsList);
			
			saveList(session, relationList);
			
			saveList(session, asmList);
			
			saveList(session, clsList);
	
			saveList(session, subclassList);
			
			saveList(session, oalList);
			
			saveList(session, xrefList);
			
			saveList(session, defXrefList);
			
			saveList(session, eqList);
			
			saveList(session, djList);
			
			saveList(session, unList);
			
			saveList(session, intList);
			
			saveList(session, infSubList);
			
			saveList(session, infSomeList);
			
			saveList(session, ontList);
			
			saveList(session, ontAnnotationList);
			
			saveList(session, ontImportsList);
		
			saveList(session, ontSubsetList);
			
			saveList(session, objSubsetList);
			
			saveList(session, annPropList);
			
			saveList(session, objAltIdList);
	
			saveList(session, annAssertionList);
			
			saveList(session, allOnlyList);
	
			saveList(session, neverSomeList);
	
			saveList(session, subRelationList);
			
			saveList(session, relDisjointList);
	
			saveList(session, relEqList);
			
			saveList(session, rcList);
			
			saveList(session, oatList);

			saveChangesHistory(wrapper.getOntologyId());
			
			if(session.isOpen())
				session.getTransaction().commit();

		//	db.getConnect().commit();
			LOG.info("Database update is completed");

		}catch(Exception ex){
			throw ex;
		}finally{
			isOperationRunning = false;
		}
		for(DbOperationsListener listener: listeners){
			listener.updateEnd();
		}
		
		
	}
	
	public void addDbOperationsListener(DbOperationsListener listener){
		if(!listeners.contains(listener))
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
	
	/**
	 * This class delete records from the database corresponding to 
	 *  the assertions removed from the ontology file.
	 *  The delete opertion is performed during the ontology update process.
	 *  The delete utility is part of the session object (to main a transaction) which saves the hibernates objects (represents new changes in the databse)
	 * @author Shahid Manzoor
	 *
	 */
	private static class DeleteUtility implements Work{
		
		//delete operation is executed for this table in the first pass
		private static Hashtable<String, String[]> tables = buildTables();
		//delete operation is executed for this table in the second pass
		private static Hashtable<String, String[]> dependentTables = buildTDependentables();
		
		private static Hashtable<String, String[]> objTablesDependency = buildObjTablesDependency();
		
	//	private PostgresDialect db ;
		//the ontology which is being updated
		private String ontology;
		private String tablePrefix;

		private static Hashtable<String, String[]> buildObjTablesDependency(){
			Hashtable<String, String[]> tables = new Hashtable<String, String[]>();
			
			tables.put("obj_subset", new String[]{"cls", "relation", "annotation_property"});
			tables.put("obj_xref", new String[]{"cls", "relation", "annotation_property"});
			tables.put("obj_alternate_id", new String[]{"cls", "relation", "annotation_property"});
			tables.put("obj_alternate_label", new String[]{"cls", "relation", "annotation_property"});
			tables.put("obj_definition_xref", new String[]{"cls", "relation", "annotation_property"});
			
			return tables;
		}
		
		private static Hashtable<String, String[]> buildTDependentables(){
			Hashtable<String, String[]> tables = new Hashtable<String, String[]>();
		
			tables.put("obj_subset", new String[]{"obj", "ontology_subset"});
			tables.put("obj_xref", new String[]{"obj", "xref"});
			tables.put("obj_alternate_id", new String[]{"obj", "id"});
			tables.put("obj_alternate_label", new String[]{"obj", "label"});
			tables.put("obj_definition_xref", new String[]{"obj", "xref"});
		
			
			return tables;
		}		
		
		
		private static Hashtable<String, String[]> buildTables(){
			Hashtable<String, String[]> tables = new Hashtable<String, String[]>();
			
			tables.put("cls", new String[]{"id", "ontology"});
			tables.put("relation", new String[]{"id", "ontology"});
			tables.put("ontology_annotation", new String[]{"ontology", "property", "annotation_value"});

			tables.put("all_only_relationship", new String[]{"ontology", "target_cls", "cls", "relation"});
			tables.put("all_some_relationship", new String[]{"ontology", "target_cls", "cls", "relation"});
			tables.put("annotation_assertion", new String[]{"ontology", "target_obj", "obj", "relation"});
			tables.put("annotation_property", new String[]{"id", "ontology"});
			tables.put("cls_intersection_of", new String[]{"ontology", "target_cls", "cls"});
			tables.put("cls_union_of", new String[]{"ontology", "target_cls", "cls"});
			tables.put("disjoint_with", new String[]{"ontology", "disjoint_cls", "cls"});
			tables.put("equivalent_to", new String[]{"ontology", "equivalent_cls", "cls"});
			tables.put("inferred_all_some_relationship", new String[]{"ontology", "target_cls", "cls", "relation"});
			tables.put("inferred_subclass_of", new String[]{"ontology", "target_cls", "cls", "relation"});
			tables.put("never_some_relationship", new String[]{"ontology", "target_cls", "cls", "relation"});
			
			//TODO: TBD
			tables.put("ontology_imports", new String[]{"ontology", "imports_ontology"});
			//TODO: special case of deletion
			tables.put("ontology_subset", new String[]{"id"});
			//TODO: TBD
			tables.put("relation_chain", new String[]{"inferred_relation", "relation1", "relation2"});
			tables.put("relation_disjoint_with", new String[]{"ontology", "disjoint_relation", "relation"});
			tables.put("relation_equivalent_to", new String[]{"ontology", "equivalent_relation", "relation"});
			tables.put("subclass_of", new String[]{"ontology", "super_cls", "cls"});
			tables.put("subrelation_of", new String[]{"ontology", "super_relation", "relation"});

			return tables;
		}
		
		public DeleteUtility(String tablePrefix, String ontology){
			this.tablePrefix = tablePrefix;
			this.ontology= ontology;
		//	db = new PostgresDialect();
			
			
		}
		
		//It deletes records from the tables 
		//which don't jave ontology column
		private void executeDependentTables(Connection connecion, Hashtable<String, String[]> tables) throws SQLException {

			Collection<String> keysSet = tables.keySet();
			for(String table: keysSet){
				String keys[] = tables.get(table);
				
				String cols = "";
				for(String key: keys){
					cols +=  table + "." + key + ", ";
				}
				
				//Connection cn = db.getConnect();
				
				//removing the last ',' character from the string
				cols = cols.substring(0, cols.length()-2);
				
				for(String onDep: objTablesDependency.get(table)){
				
					String deltaQuery = "SELECT " +  cols + " FROM " + table  + ", " + onDep
					 + " WHERE  obj = " + onDep + ".id and ontology = '" + this.ontology + "'"  +
					 " EXCEPT SELECT " +  cols + " FROM " + tablePrefix + table  + " as " + table
					 + ", " + tablePrefix + onDep
					 + " WHERE  obj = " + tablePrefix + onDep + ".id and ontology = '" + this.ontology + "'";
					
					if(DEBUG)
						LOG.debug("Find delete delta " + deltaQuery);
					
					
					ResultSet rs = connecion.createStatement().executeQuery(deltaQuery);
					
					while(rs.next()){
						String deleteQuery = "DELETE FROM " + table;
						String whereClause = " WHERE ";
						
						for(String key: keys){
							String value = rs.getString(key);
							whereClause += key + " = '" + value + "' and ";
						}
	
						whereClause = whereClause.substring(0, whereClause.length()-4);
						
						deleteQuery += whereClause;
						
						if(DEBUG)
							LOG.debug("Deleting delta " + deleteQuery);
						
						connecion.createStatement().executeUpdate(deleteQuery);
						
					}
					
					
					}
				
			}
			
		}
		
		//This method deletes records from the tables which
		//have ontology column
		private void execute(Connection connecion, Hashtable<String, String[]> tables) throws SQLException {

			Collection<String> keysSet = tables.keySet();
			for(String table: keysSet){
				String keys[] = tables.get(table);
				
				String cols = "";
				boolean containsOntologyColumn = false;
				for(String key: keys){
					cols += key + ", ";
					
					if("ontology".equals(key)){
						containsOntologyColumn = true;
					}
				}
				
				cols = cols.substring(0, cols.length()-2);
				
				String where = "";
				
				if(containsOntologyColumn)
					where = " WHERE ontology = '" + this.ontology + "' ";
				
				String deltaQuery = "SELECT " + cols + " FROM " + table  + where
				+ " EXCEPT SELECT " + cols + " FROM " + tablePrefix+table + where;
				
				if(DEBUG)
					LOG.debug("Find delete delta " + deltaQuery);
				
				ResultSet rs = connecion.createStatement().executeQuery(deltaQuery);
				
				while(rs.next()){
					String deleteQuery = "DELETE FROM " + table;
					String whereClause = " WHERE ";
					
					for(String key: keys){
						String value = rs.getString(key);
						whereClause += key + " = '" + value + "' and ";
					}

					whereClause = whereClause.substring(0, whereClause.length()-4);
					
					deleteQuery += whereClause;
					
					if(DEBUG)
						LOG.debug("Deleting delta " + deleteQuery);
					
					connecion.createStatement().executeUpdate(deleteQuery);
					
				}
				
			}
			
		}
		
		public void execute(Connection connecion) throws SQLException {
			//run second pass
			executeDependentTables(connecion, dependentTables);

			//run first pass
			execute(connecion, tables);
			
		}
		
	}
	
}
