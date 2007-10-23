package org.obo.test;

import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.impl.ForwardChainingReasoner;

import junit.framework.TestCase;

public abstract class AbstractOBOTest extends TestCase {
	protected OBOSession session;
	protected DefaultLinkDatabase linkDatabase;

	protected AbstractOBOTest(String name) {
		super(name);
	}

	public abstract Collection<String> getFilesToLoad();

	@Override
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		session = getSessionFromResources(getFilesToLoad());

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DefaultLinkDatabase(session);
	}

	protected OBOSession getSessionFromResources(Collection<String> names)
			throws DataAdapterException {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		for (String f : names) {
			String path;
			if (f.startsWith("/"))
				path = f;
			else
				path = getResourcePath() + "/" +f;
			config.getReadPaths().add(path);
		}
		config.setAllowDangling(true);
		config.setBasicSave(false);
		config.setFailFast(false);
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);
		return session;
	}
	
	protected String getResourcePath() {
		return "test_resources";
	}

	@Override
	public void tearDown() throws Exception {
		session = null;
	}

	public void testForIsA(String childID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
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
}
