package org.geneontology.gold.io.test;

import org.geneontology.gold.io.DbOperations;
import junit.framework.TestCase;

public class OntologyBulkLoaderTest extends TestCase {

	public static void testLoad() throws Exception{
		//OWLGraphWrapper wrapper = getGraphWrapper();
		
		//OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, "test_resources", "");
		
		///loader.dumpBulkLoadTables();
		
		DbOperations gold = new DbOperations();
		
		gold.bulkLoad(false);
	}

	
	public static void testIncrementalUpdate() throws Exception{
		DbOperations gold = new DbOperations();
		
	//	GeneOntologyManager.getInstance().get
		
		gold.updateGold("test_resources/caro_update.obo");
	}
	
	/*
	private static OWLGraphWrapper getGraphWrapper() throws IOException,
			OWLOntologyCreationException {

		OBOFormatParser p = new OBOFormatParser();
		OBODoc obodoc = p.parse("test_resources/caro.obo");

		Obo2Owl bridge = new Obo2Owl();
		OWLOntology ontology = bridge.convert(obodoc);

		return new OWLGraphWrapper(ontology);
	}*/
	
	

}
