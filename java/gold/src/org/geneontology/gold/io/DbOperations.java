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

public class DbOperations {


	public void bulkLoad(boolean force) throws Exception{
		bulkLoad(GeneOntologyManager.getInstance().getDefaultOboFile(), force);
	}
	
	public void bulkLoad(String oboFile, boolean force) throws Exception{
		List<String> list = dumpFiles("", oboFile);
		buildSchema(force, "");
		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
	}
	
	public List<String> dumpFiles(String tablePrefix, String oboFile) throws Exception{
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
	//	String oboFile = manager.getDefaultOboFile();
		OWLGraphWrapper wrapper = new OWLGraphWrapper( 	
			new Obo2Owl().convert(oboFile));
		
		OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, manager.getTsvFilesDir(), tablePrefix);
		return loader.dumpBulkLoadTables();
		
	}
	
	
	public void buildSchema(boolean force, String tablePrefix) throws Exception{
		SchemaManager sm = new SchemaManager();
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		sm.loadSchemaSQL(manager.getGolddbHostName(),
				manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbName(),
				manager.getOntSqlSchemaFileLocation(), tablePrefix, force);
		
	}
	
	public void loadTsvFiles(String tsvFilesDir, List<String> list) throws Exception{
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		TsvFileLoader tsvLoader = new TsvFileLoader(manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
				manager.getGolddbName());
		
		tsvLoader.loadTables(tsvFilesDir, list);
		
	}

	public void updateGold() throws Exception{
		updateGold(GeneOntologyManager.getInstance().getDefaultOboFile());
	}	
	
	public void updateGold(String oboFile) throws Exception{
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		
		List<String> list = dumpFiles(manager.getGoldDetlaTablePrefix(), oboFile);
		buildSchema(true, manager.getGoldDetlaTablePrefix());

		loadTsvFiles(GeneOntologyManager.getInstance().getTsvFilesDir(), list);
		
		GoldDeltaFactory gdf = new GoldDeltaFactory();
		
		List<Cls> clsList = gdf.buildClsDelta();
		List<Relation> relationList = gdf.buildRelationDelta();
		List<SubclassOf> subclassList = gdf.buildSubclassOfDelta();
		List<AllSomeRelationship> asmList = gdf.buildAllSomeRelationships();
		List<ObjAlternateLabel> oalList = gdf.buildObjAlternateLabels();
		
		GoldObjectFactory gof = new GoldObjectFactory();
		Session session = gof.getSession();
		
		
		for(Cls cls: clsList){
			session.merge(cls);
		}
		
		for(Relation r: relationList){
			session.merge(r);
		}
		
		for(SubclassOf sc: subclassList){
			session.merge(sc);
		}

	
		for(AllSomeRelationship asm: asmList){
			session.merge(asm);
		}
		
		for(ObjAlternateLabel oa: oalList){
			session.merge(oa);
		}
		
		session.getTransaction().commit();
	}
	
	public static void main(String args[]){
		
	}
	
}
