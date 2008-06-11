package org.oboedit.test;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOSession;

public class OBO2OBOBigReasonerTest extends OBO2OBOReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2OBOBigReasonerTest.class);
	
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
				logger.info(cmd);
				Process p = Runtime.getRuntime().exec(cmd);
				int returnVal = p.waitFor();
				long timeOfEnd = System.nanoTime();
				long timeDelta = (timeOfEnd-timeOfStart) / 1000000000;
				logger.info("t= "+timeDelta+"s");
				assertTrue("Exit value should be zero", returnVal == 0);

				logger.info("parsing: "+outFile.toString());
				session = TestUtil.getSession(outFile.toString());
				logger.info("reparsed saved results");
			}
		}	
	}
}
