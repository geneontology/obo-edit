package org.obo.test;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.CategorizedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.TermCategory;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.impl.SimpleOWLMetadataMapping;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.TermUtil;

import junit.framework.TestCase;

public abstract class AbstractOBOTest extends TestCase {
	protected OBOSession session;
	protected LinkDatabase linkDatabase;

	public AbstractOBOTest(String name) {
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
	
	public void readOBOFile(File file) throws DataAdapterException {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(file.getAbsolutePath());
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);

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
	public void testForNoIsA(String childID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		assertFalse(child.getParents().contains(
				new OBORestrictionImpl(child, OBOProperty.IS_A, parent)));
	}
	
	public void testForGenus(String childID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		boolean ok = false;
		for (Link link : child.getParents()) {
			if (TermUtil.isIntersection(link)) {
				if (link.getType().equals(OBOProperty.IS_A) &&
						link.getParent().equals(parent)) {
					ok = true;
				}
			}
				
		}
		assertTrue(ok);
	}
	public void testForDifferentium(String childID, String relID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		boolean ok = false;
		for (Link link : child.getParents()) {
			if (TermUtil.isIntersection(link)) {
				if (link.getType().getID().equals(relID) &&
						link.getParent().equals(parent)) {
					ok = true;
				}
			}
				
		}
		assertTrue(ok);
	}
	
	public void testForIsTransitive(String propID) {
		OBOProperty prop  = (OBOProperty) session.getObject(propID);
		assertTrue(prop.isTransitive());
		
	}
	public void testForLink(String childID, String relID, String parentID) {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		assertTrue(child.getParents().contains(
				new OBORestrictionImpl(child, rel, parent)));
	}

	
	public void testForCategory(String childID, String catName) {
		CategorizedObject child = (CategorizedObject) session.getObject(childID);
		TermCategory cat =session.getCategory(catName);
		assertTrue(child.getCategories().contains(cat));
	}
	
	public void testForDbxref(String childID, String xrefStr) {
		DbxrefedObject child = (DbxrefedObject) session.getObject(childID);
		boolean ok = false;
		for (Dbxref x: child.getDbxrefs()) {
			if (xrefStr.equals(x.toString())) {
				ok = true;
			}
		}
		assertTrue(ok);
	}
	
	public void testForSynonym(String id, String syn) {
		SynonymedObject io = (SynonymedObject) session.getObject(id);
		boolean ok = false;
		for (Synonym x: io.getSynonyms()) {
			if (x.getText().equals(syn)) {
				ok = true;
			}
		}
		assertTrue(ok);
	}
	
	public void testForNamespace(String id, String nsId)  {
		LinkedObject lo = (LinkedObject) session.getObject(id);
		Namespace ns = lo.getNamespace();
		assertTrue(ns.getID().equals(nsId));	
	}
	
	
	
	public void testForDefinition(String id, String def)  {
		DefinedObject lo = (DefinedObject) session.getObject(id);
		String defCurr = lo.getDefinition();
		assertTrue(defCurr.equals(def));	
	}
	
	public void testForName(String id, String name)  {
		IdentifiedObject lo =  session.getObject(id);
		assertTrue(lo.getName().equals(name));	
	}
	
	public void testInstanceType(String id, String type)  {
		Instance lo =  (Instance) session.getObject(id);
		assertTrue(lo.getType().getID().equals(type));	
	}
	
	public void testNotPresent(String id) {
		assertTrue(session.getObject(id) == null);
	}
	
	public File writeTempOBOFile() throws IOException, DataAdapterException {
		
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		File outFile = File.createTempFile("foo", "bar");
		//outFile.deleteOnExit();
		config.setWritePath(outFile.getAbsolutePath());
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		return outFile;
	}
}
