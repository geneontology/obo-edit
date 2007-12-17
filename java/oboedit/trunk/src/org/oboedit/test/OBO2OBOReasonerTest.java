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
import java.util.logging.Logger;

public class OBO2OBOReasonerTest extends TestCase {
	
	protected OBOSession session;
	
	Logger logger = Logger.getLogger("org.oboedit.test");

	
	public Collection<String> getReasonerFactoryNames() {
		String[] names={
				"org.obo.reasoner.impl.ForwardChainingReasonerFactory",
				"org.obo.reasoner.impl.LinkPileReasonerFactory"
				};
		return Arrays.asList(names);
	}

	
	// TODO - DRY : copied from AbstractOBOTest
	public void testForIsA(String childID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		System.out.println(child.getID());
		logger.info("testing for isA: "+child+" "+parent);

		for (Link link : child.getParents()) {
			System.out.println("  "+link.getParent().getID());
		}
		assertTrue(child.getParents().contains(
				new OBORestrictionImpl(child, OBOProperty.IS_A, parent)));
	}
	public void testForLink(String childID, String relID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		assertTrue(child.getParents().contains(
				new OBORestrictionImpl(child, rel, parent)));
	}



	public void testScript() throws Exception {
		runScript(false);
		runScript(true);
	}
	
	public void runScript(boolean saveAll) throws Exception {
		File testFile = new File("test_resources/camphor_catabolism.obo");
		File outFile = File.createTempFile("ccat-reasoned", ".obo");
		//outFile.deleteOnExit();
		
		for (String factoryName : getReasonerFactoryNames()) {
			logger.info("testing "+factoryName);

//			String saveFlag = saveAll ? "-realizeimpliedlinks" : "-saveimpliedlinks";
			String saveFlag = saveAll ? "-saveallimpliedlinks" : "-saveimpliedlinks";

			logger.info("saveFlag: "+saveFlag);

			String cmd = 
				"./launch_scripts/obo2obo " + testFile.getPath() + " "
				+ "-formatversion OBO_1_2 " + "-o " +
				saveFlag +
				" -reasonerfactory "
				+ factoryName + " "
				+ outFile.getPath();
			System.err.println(cmd);
			Process p = Runtime.getRuntime().exec(cmd);
			int returnVal = p.waitFor();
			assertTrue("Exit value should be zero", returnVal == 0);

			System.out.println("parsing: "+outFile.toString());
			session = TestUtil.getSession(outFile.toString());
			
			testForIsA("CHEBI:33304","CHEBI:33675"); /* asserted */
			testForIsA("GO:0019383","GO:0042178"); /* completeness */
			testForLink("testA","part_of","testB"); /* asserted */
			if (saveAll) {
				// test transitivity rels
				testForIsA("CHEBI:24974","CHEBI:23367"); /* is_a transitivity */
				testForIsA("GO:0019383","GO:0009056"); /* genus */
				testForLink("testA","part_of","testC"); /* transitivity */
				testForLink("GO:0019383","UCDHSC:results_in_division_of","CHEBI:35703"); /* differentia + transitivity */
			}
		}
	}	
}
