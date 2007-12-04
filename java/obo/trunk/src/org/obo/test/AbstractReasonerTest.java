package org.obo.test;

import java.util.Collection;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.util.ReasonerUtil;

public abstract class AbstractReasonerTest extends AbstractOBOTest {

	protected ReasonedLinkDatabase reasonedDB;
	protected LinkDatabase trimmedDB;
	
	protected LinkDatabase getLinkDatabase() {
		return reasonedDB;
	}
	
	// use LP as default: can be overridden in test
	protected static ReasonerFactory reasonerFactory =
		new LinkPileReasonerFactory();
		// new ForwardChainingReasonerFactory();

	protected AbstractReasonerTest(String name) {
		super(name);
	}
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		reasonedDB = createReasoner();
		reasonedDB.setLinkDatabase(linkDatabase);
		reasonedDB.recache();
		trimmedDB = new TrimmedLinkDatabase(reasonedDB);
		
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
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, OBOProperty.IS_A, parent);
		//System.out.println(path+" pathlen="+path.size());
	}
	
	public void testForIsAInTrimmed(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		System.out.println(trimmedDB+" testing "+child+" - "+parent);
		assertTrue(trimmedDB.getParents(child).contains(
				new OBORestrictionImpl(child, OBOProperty.IS_A, parent)));
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, OBOProperty.IS_A, parent);
		//System.out.println(path+" pathlen="+path.size());
	}


	public void testForLink(String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		System.out.println(reasonedDB+" testing "+child+" "+rel+" "+parent);
		assertTrue(reasonedDB.hasRelationship(child, rel, parent) != null);
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, OBOProperty.IS_A, parent);
		//System.out.println(path+" pathlen="+path.size());

	}
	
	public void testForLinkInTrimmed(String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		System.out.println(trimmedDB+" testing "+child+" "+rel+" "+parent);
		assertTrue(trimmedDB.getParents(child).contains(
				new OBORestrictionImpl(child, rel, parent)));

	}


}
