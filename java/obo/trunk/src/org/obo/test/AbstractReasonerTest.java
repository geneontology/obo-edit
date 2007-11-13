package org.obo.test;

import org.obo.datamodel.*;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

public abstract class AbstractReasonerTest extends AbstractOBOTest {

	protected ReasonedLinkDatabase reasonedDB;
	
	protected static ReasonerFactory reasonerFactory =
		new ForwardChainingReasonerFactory();

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
		return reasonerFactory.createReasoner();
	}

	public static ReasonerFactory getReasonerFactory() {
		return reasonerFactory;
	}

	public static void setReasonerFactory(ReasonerFactory reasonerFactoryIn) {
		reasonerFactory = reasonerFactoryIn;
	}

	public void testForIsA(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		System.out.println(reasonedDB+" testing "+child+" - "+parent);
		assertTrue(reasonedDB.hasRelationship(child, OBOProperty.IS_A, parent) != null);
	}

	public void testForLink(String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		assertTrue(reasonedDB.hasRelationship(child, rel, parent) != null);
	}


}
