package org.geneontology.gold.io.test;

import org.geneontology.conf.GoConfigManager;
import org.geneontology.gold.io.DbOperations;
import junit.framework.TestCase;

public class OntologyBulkLoaderTest extends TestCase {

	public static void testLoad() throws Exception{

		GoConfigManager.getInstance().setProperty("geneontology.gold.db", "goldtest");
		
		DbOperations gold = new DbOperations();
		
		gold.bulkLoad(true);
	}

	
	public static void testIncrementalUpdate() throws Exception{
		GoConfigManager.getInstance().setProperty("geneontology.gold.db", "goldtest");

		DbOperations gold = new DbOperations();
		
		gold.update("test_resources/caro_update.obo");
	}
	
	
	

}
