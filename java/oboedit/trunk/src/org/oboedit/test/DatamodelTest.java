package org.oboedit.test;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;

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
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
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
				Iterator it2;

				it2 = lio.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					Link slink = null;
					Iterator it3 = slio.getParents().iterator();
					while (it3.hasNext()) {
						Link temp = (Link) it3.next();
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
				it2 = slio.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
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
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Iterator it2 = lo.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					boolean found = false;
					Iterator it3 = link.getParent().getChildren().iterator();
					while (it3.hasNext()) {
						Link tlink = (Link) it3.next();
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
				it2 = lo.getChildren().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					boolean found = false;
					Iterator it3 = link.getChild().getParents().iterator();
					while (it3.hasNext()) {
						Link tlink = (Link) it3.next();
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
