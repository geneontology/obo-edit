package org.obo.test;

import org.obo.datamodel.*;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.ReasonedLinkDatabase;

public abstract class AbstractReasonerTest extends AbstractOBOTest {

	protected ReasonedLinkDatabase reasonedDB;

	protected AbstractReasonerTest(String name) {
		super(name);
	}
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		reasonedDB = createReasoner();
		reasonedDB.setLinkDatabase(linkDatabase);
		reasonedDB.recache();
	}
	
	protected ReasonedLinkDatabase createReasoner() {
		return new ForwardChainingReasoner();
	}

	public void testForIsA(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		System.out.println("testing "+child+" - "+parent);
		assertTrue(reasonedDB.hasRelationship(child, OBOProperty.IS_A, parent) != null);
	}

	public void testForLink(String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		assertTrue(reasonedDB.hasRelationship(child, rel, parent) != null);
	}


}
