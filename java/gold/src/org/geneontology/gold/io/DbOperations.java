package org.geneontology.gold.io;

import java.util.List;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.factory.GoldDeltaFactory;
import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
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


	/**
	 * Loads the contents of the obo file whose path is supplied 
	 * the geneontology.gold.obofil property.
	 * @param force
	 * @throws Exception
	 */
	public void bulkLoad(boolean force) throws Exception{
		bulkLoad(GeneOntologyManager.getInstance().getDefaultOboFile(), force);
	}
	
	/**
	 * Load the contents of the obo file into GOLD
	 * @param oboFile: The path of the obo file
	 * @param The true value of the force parameter drops all the existing tables
	 * 			creates new ones.
	 * @throws Exception
	 */
	public void bulkLoad(String oboFile, boolean force) throws Exception{
		List<String> list = dumpFiles("", oboFile);
		buildSchema(force, "");
		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
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
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
	//	String oboFile = manager.getDefaultOboFile();
		OWLGraphWrapper wrapper = new OWLGraphWrapper( 	
			new Obo2Owl().convert(oboFile));
		
		OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, manager.getTsvFilesDir(), tablePrefix);
		return loader.dumpBulkLoadTables();
		
	}
	
	/**
	 * It creates schema of GOLD database.
	 * @param force: The true value of this parameter drop of all existing tables and
	 * 			creates new ones.
	 * @param tablePrefix This prefix is used as prefix of each table created in the database.
	 * @throws Exception
	 */
	public void buildSchema(boolean force, String tablePrefix) throws Exception{
		SchemaManager sm = new SchemaManager();
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		sm.loadSchemaSQL(manager.getGolddbHostName(),
				manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbName(),
				manager.getOntSqlSchemaFileLocation(), tablePrefix, force);
		
	}
	
	/**
	 * It loads the TSV files in the GOLD database.
	 * @param tsvFilesDir The directory where TSV files are residing
	 * @param list It is list of the names (without extension) of the files 
	 * 		to be loaded in the GOLD database
	 * @throws Exception
	 */
	public void loadTsvFiles(String tsvFilesDir, List<String> list) throws Exception{
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		TsvFileLoader tsvLoader = new TsvFileLoader(manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
				manager.getGolddbName());
		
		tsvLoader.loadTables(tsvFilesDir, list);
		
	}

	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located the default location.
	 * @throws Exception
	 */
	public void updateGold() throws Exception{
		updateGold(GeneOntologyManager.getInstance().getDefaultOboFile());
	}	
	
	/**
	 * Incrementa update of the GOLD database from the contents of the obo file
	 * located at the path supplied in the parameter
	 * @param oboFile
	 * @throws Exception
	 */
	public void updateGold(String oboFile) throws Exception{
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		
		List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), oboFile);
		buildSchema(true, manager.getGoldDetlaTablePrefix());

		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
		
		GoldDeltaFactory gdf = new GoldDeltaFactory();

		List<SubclassOf> subclassList = gdf.buildSubclassOfDelta();
		List<Relation> relationList = gdf.buildRelationDelta();
		List<AllSomeRelationship> asmList = gdf.buildAllSomeRelationships();
		List<Cls> clsList = gdf.buildClsDelta();
		List<ObjAlternateLabel> oalList = gdf.buildObjAlternateLabels();
		
		
		//close the session associated with the tables prefixed with 
		// the value of the geneontology.gold.deltatableprefix property
		gdf.getSession().close();
		
		GoldObjectFactory gof = GoldObjectFactory.buildDefaultFactory();
		Session session = gof.getSession();
		session.clear();

		for(Cls cls: clsList){
			session.saveOrUpdate(cls);
		}

		for(Relation r: relationList){
			session.saveOrUpdate(r);
		}
		
		for(SubclassOf sc: subclassList){
			session.saveOrUpdate(sc);
		}

		for(AllSomeRelationship asm: asmList){
			session.saveOrUpdate(asm);
		}

		
		
		for(ObjAlternateLabel oa: oalList){
			session.saveOrUpdate(oa);
		}

		session.getTransaction().commit();
		
	}
	
}
