package org.oboedit.test;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOSession;

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
			logger.info(cmd);
			Process p = Runtime.getRuntime().exec(cmd);
			int returnVal = p.waitFor();
			assertTrue("Exit value should be zero", returnVal == 0);

		}
	}	
}
