package org.oboedit.test;

import java.io.*;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.SerialAdapter;
import org.obo.datamodel.OBOSession;

import junit.framework.*;

import org.apache.log4j.*;

public class SimpleSerialRoundtripTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleSerialRoundtripTest.class);

	public String[] testFiles = { "test_resources/testfile.1.0.obo" };

	public void testRoundtrip() throws DataAdapterException, IOException {
		for (int i = 0; i < testFiles.length; i++) {
			OBOFileAdapter adapter = new OBOFileAdapter();
			OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.getReadPaths().add(testFiles[i].toString());
			OBOSession session = (OBOSession) adapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, config, null);

			SerialAdapter serialadapter = new SerialAdapter();
			FileAdapterConfiguration serialconfig = new FileAdapterConfiguration();
			File outFile = File.createTempFile("test", "serial");
			outFile.deleteOnExit();
			serialconfig.setWritePath(outFile.getAbsolutePath());
			serialadapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, serialconfig, session);

			serialconfig = new FileAdapterConfiguration();
			serialconfig.getReadPaths().add(outFile.getAbsolutePath());
			OBOSession session2 = (OBOSession) serialadapter.doOperation(
					OBOAdapter.READ_ONTOLOGY, serialconfig, null);
			TestUtil.sessionCheck(this, session, session2);

			outFile.delete();
		}
	}
}
