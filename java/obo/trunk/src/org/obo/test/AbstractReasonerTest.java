package org.obo.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.filters.Filter;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.ReasonerUtil;

public abstract class AbstractReasonerTest extends AbstractOBOTest {

	protected ReasonedLinkDatabase reasonedDB;
	protected LinkDatabase trimmedDB;
	
	
	//initialize logger
	protected final static Logger logger = Logger.getLogger(AbstractReasonerTest.class);

	
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
	
	public void reReason(ReasonerFactory fac) {
		setReasonerFactory(fac);
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
		testForLink(childID, OBOProperty.IS_A.getID(), parentID);
	}
	
	public void testForNoIsA(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		logger.info(reasonedDB+" testing "+child+" - "+parent);
		assertTrue(reasonedDB.hasRelationship(child, OBOProperty.IS_A, parent) == null);
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, OBOProperty.IS_A, parent);
		//logger.info(path+" pathlen="+path.size());
	}
	
	public void testForIsAInTrimmed(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		logger.info(trimmedDB+" testing "+child+" - "+parent);
		assertTrue(trimmedDB.getParents(child).contains(
				new OBORestrictionImpl(child, OBOProperty.IS_A, parent)));
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, OBOProperty.IS_A, parent);
		//logger.info(path+" pathlen="+path.size());
	}
	
	public void testForRedundantIsA(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		logger.info(reasonedDB+" testing redundant isa "+child+" - "+parent);
		for (Link link : reasonedDB.getParents(child)) {
			if (link.getParent().equals(parent) && link.getType().equals(OBOProperty.IS_A)) {
				assertTrue(ReasonerUtil.isRedundant(reasonedDB, link));
				return;
			}
		}
		assertTrue(false);
	}
	



	public void testForLink(String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		logger.info(reasonedDB+" testing "+child+" "+rel+" "+parent);
		assertTrue(reasonedDB.hasRelationship(child, rel, parent) != null);
		Link link = new OBORestrictionImpl(child,rel,parent);
		logger.info("inferred="+link);
		for (Explanation exp : reasonedDB.getExplanations(link)) {
			logger.info("  exp="+exp);
		}
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, rel, parent); // TODO - Cycle
		//logger.info(path+" pathlen="+path.size());

	}
	
	public void testForLinkInTrimmed(String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		logger.info(trimmedDB+" testing "+child+" "+rel+" "+parent);
		assertTrue(trimmedDB.getParents(child).contains(
				new OBORestrictionImpl(child, rel, parent)));

	}
	
	public Collection<Link> filterReasonedLinks(Filter filter) {
		Collection<Link> matches = 
			new LinkedList<Link>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject)io;
				for (Link link : reasonedDB.getParents(lo)) {
					if (filter.satisfies(link)) {
						matches.add(link);
					}
				}
			}
		}
		return matches;
	}



}
