package org.oboedit.test;

import junit.framework.*;
import java.io.*;
import java.util.Arrays;
import java.util.Collection;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.test.AbstractReasonerTest;

import org.apache.log4j.*;

public class OBO2OWLTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2OWLTest.class);
	
	protected OBOSession session;
	

	
	public Collection<String> getReasonerFactoryNames() {
		String[] names={
				"org.obo.reasoner.impl.ForwardChainingReasonerFactory",
				"org.obo.reasoner.impl.LinkPileReasonerFactory"
				};
		return Arrays.asList(names);
	}

	
	


	public void testScript() throws Exception {
		runScript(false);
	}
	
	public void runScript(boolean saveAll) throws Exception {
		File testFile = new File("test_resources/camphor_catabolism.obo");
		File outFile = File.createTempFile("ccat", ".owl");
		//outFile.deleteOnExit();
		
		for (String factoryName : getReasonerFactoryNames()) {
			logger.info("testing "+factoryName);


			String cmd = 
				"./launch_scripts/obo2owl " + testFile.getPath() + " "
				+ "-o "
				+ outFile.getPath();
			logger.error(cmd);
			Process p = Runtime.getRuntime().exec(cmd);
			int returnVal = p.waitFor();
			assertTrue("Exit value should be zero", returnVal == 0);

		}
	}	
}
