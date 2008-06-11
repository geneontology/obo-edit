package org.oboedit.test;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOSession;
import org.obo.test.AbstractOBOTest;

public class OBO2OBOSemanticParserTest extends AbstractOBOTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2OBOSemanticParserTest.class);

	public OBO2OBOSemanticParserTest(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	protected OBOSession session;
	String file = "regulation_of_somitogenesis.obo";



	public void testScript() throws Exception {
		runScript(false);
	}

	public void runScript(boolean saveAll) throws Exception {
		File testFile = new File("test_resources/"+file);
		File outFile = File.createTempFile("ccat", ".obo");
		//outFile.deleteOnExit();

		String cmd = 
			"./launch_scripts/obo2obo -allowdangling -semanticparse -addsynonyms " + testFile.getPath() + " "
			+ "-o -saveimpliedlinks "
			+ outFile.getPath();
		logger.info(cmd);
		Process p = Runtime.getRuntime().exec(cmd);
		int returnVal = p.waitFor();
		assertTrue("Exit value should be zero", returnVal == 0);
		readOBOFile(outFile);
		String id = "GO:0014807"; // regulation of somitogenesis
		testForGenus(id,"GO:0065007"); /* asserted */
		testForDifferentium(id,"regulates", "GO:0001756"); /* asserted */
		testForIsA(id, "GO:0045995"); /* regulation of embryonic development */

	}

	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={};
		return Arrays.asList(files);
	}
	
}
