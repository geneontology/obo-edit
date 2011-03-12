package org.geneontology.gold.io;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.factory.GoldDeltaFactory;
import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.AllOnlyRelationship;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.AnnotationAssertion;
import org.geneontology.gold.hibernate.model.AnnotationProperty;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ClsIntersectionOf;
import org.geneontology.gold.hibernate.model.ClsUnionOf;
import org.geneontology.gold.hibernate.model.DisjointWith;
import org.geneontology.gold.hibernate.model.EquivalentTo;
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
 * This class provides adminstrative operations for the GOLD database.
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
	
	private boolean dbCreate;
	
	public DbOperations(){
		listeners = new ArrayList<DbOperationsListener>();
	}
	
	public List<Ontology> getLastUpdateStatus(){
		GoldObjectFactory factory = GoldObjectFactory.buildDefaultFactory();
		
		return factory.getOntologies();
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
		List files = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
		
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
	public void bulkLoad(String ontologyLocation, boolean force) throws Exception{
		if(DEBUG)
			LOG.debug(ontologyLocation);
		
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}
		
		
		_bulkLoad(buildOWLGraphWrapper(ontologyLocation), force);
	}
	


	private void _bulkLoad(OWLGraphWrapper wrapper, boolean force) throws Exception{

		if(DEBUG){
			LOG.debug("Bulk Load for: " + wrapper.getOntologyId());
		}
		
		if(isOperationRunning){
			throw new RuntimeException("Another operating is still running. ");
		}
		
		try{
			isOperationRunning = true;
	
			List<String> list = dumpFiles("", wrapper);
			
			if(!dbCreate)
				buildSchema(force, "");
			
			dbCreate = true;
			loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
			
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
	
	public void bulkLoad(OWLGraphWrapper wrapper, boolean force) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.bulkLoadStart();
		}

		_bulkLoad(wrapper, force);
	 
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
		return dumpFiles(tablePrefix, buildOWLGraphWrapper(oboFile));
	}
	
	/**
	 * 
	 * @return This method build a new instance of the {@link OWLGraphWrapper} class
	 * @throws IOException 
	 * @throws OWLOntologyCreationException 
	 */
	public OWLGraphWrapper[] buildOWLGraphWrappers() throws IOException, OWLOntologyCreationException{
		
		List  list = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
		
		OWLGraphWrapper graph[] = new OWLGraphWrapper[list.size()];
		
		for(int i=0;i<list.size();i++){
			graph[i] = buildOWLGraphWrapper(list.get(i).toString());
		}
	
		
		return graph;
		
	}

	public OWLOntology buildOWLOntology(String ontologyLocation) throws IOException, OWLOntologyCreationException{
		if(ontologyLocation == null)
			throw new FileNotFoundException("The file is not found");
		

		for(DbOperationsListener listener: listeners){
			listener.startOntologyLoad();
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
			listener.endOntologyLoad(ontology);
		}
		
		return ontology;
	}	
	
	
	public OWLGraphWrapper buildOWLGraphWrapper(String ontologyLocation) throws IOException, OWLOntologyCreationException{
		/*if(ontologyLocation == null)
			throw new FileNotFoundException("The file is not found");
		

		for(DbOperationsListener listener: listeners){
			listener.startOntologyLoad();
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
				
		}*/
		OWLOntology ontology = buildOWLOntology(ontologyLocation);
		
		OWLGraphWrapper wrapper = new OWLGraphWrapper(ontology);

		/*
		for(DbOperationsListener listener: listeners){
			listener.endOntologyLoad(wrapper);
		}*/
		
		return wrapper;
	}	
	
	public List<String> dumpFiles(String tablePrefix, OWLGraphWrapper wrapper) throws Exception{
		for(DbOperationsListener listener: listeners){
			listener.dumpFilesStart();
		}
		
		if(DEBUG){
			LOG.debug("-");
		}
		
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		/*OWLGraphWrapper wrapper = new OWLGraphWrapper( 	
			new Obo2Owl().convert(oboFile));
		*/
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
		
		if(DEBUG){
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
	
		
		if(DEBUG){
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
		
		if(DEBUG){
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
		if(DEBUG){
			LOG.debug("-");
		}

		List list = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
		
		if(list == null || list.size()==0){
			throw new Exception("Ontology File Locations are not specified in the geneontology.gold.ontologylocation property" );
		}
		
		for(Object obj: list)
			update(obj.toString());
	}	
	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void update(String oboFile) throws Exception{
		
		updateGold(buildOWLGraphWrapper(oboFile));
		
	}
	
	public void updateGold(OWLGraphWrapper wrapper) throws Exception{
		if(DEBUG){
			LOG.debug("-");
		}

		for(DbOperationsListener listener: listeners){
			listener.updateStart();
		}
	
		if(isOperationRunning){
			throw new RuntimeException("Another operating is still running. ");
		}
		
		isOperationRunning = true;
		
		try{
			GeneOntologyManager manager = GeneOntologyManager.getInstance();
			
			List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), wrapper);
			buildSchema(true, manager.getGoldDetlaTablePrefix());
	
			loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
			
			GoldDeltaFactory gdf = new GoldDeltaFactory();
	
			if(DEBUG){
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
			//close the session associated with the tables prefixed with 
			// the value of the geneontology.gold.deltatableprefix property
			gdf.getSession().close();
			
			if(DEBUG){
				LOG.debug("Merging the the delta objects in the GOLD database");
			}
			
			
			
			GoldObjectFactory gof = GoldObjectFactory.buildDefaultFactory();
			Session session = gof.getSession();
			session.clear();
	
			//delete the removed assertions
			//TODO: implement it for mult ontologies
			DeleteUtility du = new DeleteUtility(manager.getGoldDetlaTablePrefix(), wrapper.getOntologyId());
			session.doWork(du);
			
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
			
			LOG.info("Database update is completed");
			
			session.getTransaction().commit();
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
	 * This class delete corresponding records of
	 *  the removed assertions in the obo file during the update process
	 * @author Shahid Manzoor
	 *
	 */
	private static class DeleteUtility implements Work{
		
		//delete operation is executed for this table in the first pass
		private static Hashtable<String, String[]> tables = buildTables();
		//delete operation is executed for this table in the second pass
		private static Hashtable<String, String[]> dependentTables = buildTDependentables();
		
		private static Hashtable<String, String[]> objTablesDependency = buildObjTablesDependency();
		
		
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

			//run first pass
			execute(connecion, tables);
			//run second pass
			executeDependentTables(connecion, dependentTables);
			
		}
		
	}
	
}
