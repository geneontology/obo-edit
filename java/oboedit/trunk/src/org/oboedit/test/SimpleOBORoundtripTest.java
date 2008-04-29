package org.oboedit.test;

import java.io.*;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryList;

import junit.framework.*;

public class SimpleOBORoundtripTest extends TestCase {

	public String[] testFiles = { "test_resources/testfile.1.0.obo" };

	public void testRoundtrip() throws DataAdapterException, IOException {
		for (int i = 0; i < testFiles.length; i++) {
			OBOFileAdapter adapter = new OBOFileAdapter();
			/*
			 * URL testFile = ClassLoader.getSystemClassLoader().
			 * getResource(testFiles[i]);
			 */
			OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.getReadPaths().add(testFiles[i].toString());
			OBOSession session = (OBOSession) adapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, config, null);
			config = new OBOFileAdapter.OBOAdapterConfiguration();
			File outFile = File.createTempFile("test", "obo");
			outFile.deleteOnExit();
			config.setWritePath(outFile.getAbsolutePath());
			adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);

			config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.getReadPaths().add(outFile.getAbsolutePath());
			OBOSession session2 = (OBOSession) adapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, config, null);

			// get the history generator version of the changes
			HistoryList allChanges = HistoryGenerator.getHistory(session,
					session2);

			assertTrue(
					"The file should be exactly the same going in as going out; allChanges = "
							+ allChanges,
					allChanges.size() == 0);
			outFile.delete();
		}
	}
}
