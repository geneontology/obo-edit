package org.oboedit.test;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.apache.log4j.Logger;

public class OBO2FlatTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2FlatTest.class);

	public void testScript() throws IOException, InterruptedException {
		File processFile = File.createTempFile("process", ".ontology");
		File functionFile = File.createTempFile("function", ".ontology");
		File componentFile = File.createTempFile("component", ".ontology");
		File defsFile = File.createTempFile("GODEFS", "defs");

		File originalProcessFile = new File("lib/resources/process.ontology");
		File originalFunctionFile = new File("lib/resources/function.ontology");
		File originalComponentFile = new File(
				"lib/resources/component.ontology");
		File originalDefsFile = new File("lib/resources/GO.defs");

		Process p = Runtime.getRuntime().exec(
				"./obo2flat " + "lib/resources/testfile.1.0.obo "
						+ "--gopresets " + processFile.getPath() + " "
						+ componentFile.getPath() + " "
						+ functionFile.getPath() + " " + defsFile.getPath());
		int returnVal = p.waitFor();
		assertTrue("Exit value should be zero", returnVal == 0);

		TestUtil.fileCheck(this, processFile, originalProcessFile);
		TestUtil.fileCheck(this, componentFile, originalComponentFile);
		TestUtil.fileCheck(this, functionFile, originalFunctionFile);
		TestUtil.fileCheck(this, defsFile, originalDefsFile);

		processFile.delete();
		functionFile.delete();
		componentFile.delete();
		defsFile.delete();
	}
}
