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

public class OBO2OBOBigReasonerTest extends OBO2OBOReasonerTest {
	
	protected OBOSession session;
	
	public Collection<String> getOntologyURLs() {
		String[] names={
				"http://purl.org/obo/obo-all/go_xp_regulation/go_xp_regulation.obo",
				"http://purl.org/obo/obo-all/go_xp_chebi/go_xp_chebi.imports.obo",
				"http://purl.org/obo/obo-all/mammalian_phenotype_xp/mammalian_phenotype_xp.imports.obo"
				};
		return Arrays.asList(names);
	}

	
	public void testScript() throws Exception {
		runScript(false);
	}
	
	public void runScript(boolean saveAll) throws Exception {
		File outFile = File.createTempFile("reasoner-out", ".obo");
		//outFile.deleteOnExit();
		
		for (String url : getOntologyURLs()) {
			logger.info("ontology: "+url);

			for (String factoryName : getReasonerFactoryNames()) {
				logger.info("testing "+factoryName);

				String saveFlag = saveAll ? "-saveallimpliedlinks" : "-saveimpliedlinks";

				long timeOfStart = System.nanoTime();

				String cmd = 
					"./launch_scripts/obo2obo -allowdangling " + url + " "
					+ "-formatversion OBO_1_2 " + "-o " +
					saveFlag +
					" -reasonerfactory "
					+ factoryName + " "
					+ outFile.getPath();
				System.err.println(cmd);
				Process p = Runtime.getRuntime().exec(cmd);
				int returnVal = p.waitFor();
				long timeOfEnd = System.nanoTime();
				long timeDelta = (timeOfEnd-timeOfStart) / 1000000000;
				System.err.println("t= "+timeDelta+"s");
				assertTrue("Exit value should be zero", returnVal == 0);

				System.out.println("parsing: "+outFile.toString());
				session = TestUtil.getSession(outFile.toString());
				System.err.println("reparsed saved results");
			}
		}	
	}
}
