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
import org.obo.test.AbstractOBOTest;
import org.obo.test.AbstractReasonerTest;
import org.apache.log4j.*;

public class OBO2OBOSemanticParserTest extends AbstractOBOTest {

	public OBO2OBOSemanticParserTest(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	protected OBOSession session;
	String file = "regulation_of_somitogenesis.obo";
	Logger logger = Logger.getLogger("org.oboedit.test");



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
		System.err.println(cmd);
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
