package org.oboedit.test;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryList;

public class SimpleOBORoundtripTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleOBORoundtripTest.class);

	public String[] testFiles = { "test_resources/testfile.1.0.obo", "test_resources/testfile.1.2.obo" };

	public void testRoundtrip() throws DataAdapterException, IOException {
		for (int i = 0; i < testFiles.length; i++) {
			OBOFileAdapter adapter = new OBOFileAdapter();
			OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.getReadPaths().add(testFiles[i].toString());
			OBOSession session = (OBOSession) adapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, config, null);
			config = new OBOFileAdapter.OBOAdapterConfiguration();
			File outFile = File.createTempFile("test", ".obo");
			// Save 1.0 input file as as 1.0 and 1.2 as 1.2
			if (testFiles[i].indexOf("1.0") > 0)
				config.setSerializer("OBO_1_0");
			else if (testFiles[i].indexOf("1.2") > 0)
				config.setSerializer("OBO_1_2");
			config.setWritePath(outFile.getAbsolutePath());
			adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);

			// Read in the round-tripped file
			config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.getReadPaths().add(outFile.getAbsolutePath());
			OBOSession session2 = (OBOSession) adapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, config, null);

			// get the history generator version of the changes
			HistoryList allChanges = HistoryGenerator.getHistory(session, session2);

			assertTrue("The file should be exactly the same going in as going out; in = " + 
				   testFiles[i] + ", out = " + outFile + ", allChanges = " + allChanges,
				   allChanges.size() == 0);
		}
	}
}
