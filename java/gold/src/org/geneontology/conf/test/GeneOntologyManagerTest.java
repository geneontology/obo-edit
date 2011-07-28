package org.geneontology.conf.test;

import org.apache.commons.configuration.ConfigurationException;
import org.geneontology.conf.GoConfigManager;

import junit.framework.TestCase;

public class GeneOntologyManagerTest extends TestCase {

	
	public static void testGoldDbConfigurations() throws ConfigurationException{
		GoConfigManager manager = GoConfigManager.getInstance();

		assertNotNull(manager.getGolddbName());

		assertNotNull(manager.getGolddbHostName());
	
		assertNotNull(manager.getGolddbUserName());
	
		assertNotNull(manager.getGolddbUserPassword());

	}
	
	
}
