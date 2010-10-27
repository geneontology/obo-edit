package org.geneontology.conf.test;

import org.apache.commons.configuration.ConfigurationException;
import org.geneontology.conf.GeneOntologyManager;

import junit.framework.TestCase;

public class GeneOntologyManagerTest extends TestCase {

	
	public static void testGoldDbConfigurations() throws ConfigurationException{
		GeneOntologyManager manager = GeneOntologyManager.getInstance();

		assertNotNull(manager.getGolddbName());

		assertNotNull(manager.getGolddbHostName());
	
		assertNotNull(manager.getGolddbUserName());
	
		assertNotNull(manager.getGolddbUserPassword());

	}
	
	
}
