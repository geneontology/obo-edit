package org.oboedit.test;

import junit.framework.*;
import java.io.*;

import org.obo.datamodel.OBOSession;

public class OBO2OBOTest extends TestCase {

	public void testScript() throws Exception {
		File testFile = new File("lib/resources/testfile.1.0.obo");
		File out12File = File.createTempFile("gene_ontology_1_2", ".obo");
		File out10File = File.createTempFile("gene_ontology_1_0", ".obo");
		out12File.deleteOnExit();
		out10File.deleteOnExit();

		Process p = Runtime.getRuntime().exec(
				"./obo2obo " + testFile.getPath() + " "
						+ "-formatversion OBO_1_2 " + "-o "
						+ out12File.getPath());
		int returnVal = p.waitFor();
		assertTrue("Exit value should be zero", returnVal == 0);
		p = Runtime.getRuntime().exec(
				"./obo2obo " + "-formatversion OBO_1_0 " + out12File.getPath()
						+ " " + "-o " + out10File.getPath());
		returnVal = p.waitFor();
		assertTrue("Exit value should be zero", returnVal == 0);

		// TestUtil.fileCheck(this, testFile, out10File);
		OBOSession testSession = TestUtil.getSession(testFile.toString());
		OBOSession temp = TestUtil.getSession(out10File.toString());

		TestUtil.sessionCheck(this, testSession, temp);

		temp = TestUtil.getSession(out12File.toString());
		TestUtil.sessionCheck(this, testSession, temp);

		out12File.delete();
		out10File.delete();
	}
}
