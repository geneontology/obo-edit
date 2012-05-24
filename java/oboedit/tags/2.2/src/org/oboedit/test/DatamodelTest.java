package org.oboedit.test;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.SerialAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;

public class DatamodelTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DatamodelTest.class);

	public static OBOSession createSerialSession(OBOSession session)
			throws DataAdapterException, IOException {
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
		outFile.delete();
		return session2;
	}

	protected OBOSession session;
	protected OBOSession serialSession;

	@Override
	public void setUp() throws Exception {
		createSessions();
	}

	protected void createSessions() throws DataAdapterException, IOException {
		session = TestUtil.createSession();
		serialSession = createSerialSession(session);
	}

	/*
	 * public void testLinks() throws DataAdapterException, IOException {
	 * doLinkTest(session); doLinkTest(serialSession); }
	 */
	public void testObjectContinuity() {
		assertTrue(
				"OBO and Serial versions of ontology should have the same number of objects",
				session.getObjects().size() == session.getObjects().size());
		for(IdentifiedObject io : session.getObjects()) {
			IdentifiedObject sio = serialSession.getObject(io.getID());
			assertNotNull("Serial session did not contain " + io, sio);
			assertTrue("Serial version of " + io + " should equal obo version",
					io.equals(sio));
			assertTrue("Serial version of " + io
					+ " should have same class as obo version", io.getClass()
					.equals(sio.getClass()));
			assertTrue("Serial version of " + io
					+ " should have the same hash as obo version", io
					.hashCode() == sio.hashCode());
			if (io instanceof LinkedObject) {
				LinkedObject lio = (LinkedObject) io;
				LinkedObject slio = (LinkedObject) sio;
				assertTrue("Serial version of " + io
						+ " should have same children as obo version", lio
						.getChildren().equals(slio.getChildren()));
				assertTrue("Serial version of " + io
						+ " should have same parents as obo version", lio
						.getParents().equals(slio.getParents()));
				for (Link link : lio.getParents()) {
					Link slink = null;
					for(Link temp : slio.getParents()) {
						if (temp.equals(link)) {
							slink = temp;
							break;
						}
					}
					assertNotNull("Serial version of " + link
							+ " should give true equals() response", slink);
					assertTrue("Serial version of " + link
							+ " and OBO version should have same hash codes",
							slink.hashCode() == link.hashCode());
					assertTrue(
							"Serial version of "
									+ link
									+ " should be contained in parents of OBO version of "
									+ slio + ", slio.getParents() = "
									+ slio.getParents(), slio.getParents()
									.contains(link));
				}
				for (Link link : slio.getParents()) {
					assertTrue(
							"OBO version of "
									+ link
									+ " should be contained in parents of serial version of "
									+ lio, lio.getParents().contains(link));
				}
				assertTrue("Serial version of " + lio
						+ " should have the same hash as obo version", lio
						.hashCode() == slio.hashCode());
			}
		}
	}

	protected void doLinkTest(OBOSession session) throws DataAdapterException,
			IOException {
		for(IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				for(Link link : lo.getParents()) {
					boolean found = false;
					for(Link tlink : link.getParent().getChildren()) {
						if (tlink.equals(link)) {
							assertTrue(
									"Twinned parent child links should be equal AND identical: "
											+ link, tlink == link);
							found = true;
						}
					}
					assertTrue(
							"Parent links should be twinned with child links: "
									+ link, found);
				}
				for(Link link : lo.getChildren()) {
					boolean found = false;
					for(Link tlink : link.getChild().getParents()) {
						if (tlink.equals(link)) {
							assertTrue(
									"Twinned parent child links should be equal AND identical: "
											+ link, tlink == link);
							found = true;
						}
					}
					assertTrue(
							"Parent links should be twinned with child links: "
									+ link, found);
				}
			}
		}
	}
}
