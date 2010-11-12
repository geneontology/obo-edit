package org.geneontology.gold.io.test;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.DbOperations;
import junit.framework.TestCase;

public class OntologyBulkLoaderTest extends TestCase {

	public static void testLoad() throws Exception{
		
		DbOperations gold = new DbOperations();
		
		gold.bulkLoad(true);
	}

	
	public static void testIncrementalUpdate() throws Exception{
		DbOperations gold = new DbOperations();
		
		GeneOntologyManager.getInstance().setProperty("geneontology.gold.db", "goldtest");
		gold.updateGold("test_resources/caro_update.obo");
	}
	
	
	

}
