package org.obo.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOSerializationEngine;
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
import org.obo.reasoner.rbr.RuleBasedReasonerFactory;
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
		new RuleBasedReasonerFactory();
	//new LinkPileReasonerFactory();
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

	public Collection<Link> getLinks(LinkDatabase ldb, String childID, String relID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		OBOProperty rel = (OBOProperty) session.getObject(relID);
		Collection<Link> links = new ArrayList<Link>();
		for (Link link : ldb.getParents(child)) {
			if (link.getType().equals(rel) && link.getParent().equals(parent)) {
				links.add(link);
			}
		}
		return links;
	}


	public Collection<Link> getLinks(String childID, String relID, String parentID)  {
		return getLinks(reasonedDB, childID,  relID, parentID);
	}

	public boolean hasLink(LinkDatabase ldb, String childID, String relID, String parentID)  {
		return getLinks(ldb,childID, relID, parentID).size() > 0;
	}
	public boolean hasLink(String childID, String relID, String parentID)  {
		return hasLink(reasonedDB, childID, relID, parentID);
	}
	public boolean hasIsALink(LinkDatabase ldb, String childID, String parentID)  {
		return hasLink(ldb, childID, OBOProperty.IS_A.getID(), parentID);
	}
	public boolean hasIsALink(String childID, String parentID)  {
		return hasLink(childID, OBOProperty.IS_A.getID(), parentID);
	}


	public void testForIsA(String childID, String parentID)  {
		testForLink(childID, OBOProperty.IS_A.getID(), parentID);
	}

	public void testForNoIsA(String childID, String parentID)  {
		assertFalse(hasIsALink(childID, parentID));
	}

	public void testForIsAInTrimmed(String childID, String parentID)  {
		assertTrue(hasIsALink(trimmedDB, childID, parentID));
	}

	public void testForNoIsAInTrimmed(String childID, String parentID)  {
		assertFalse(hasIsALink(trimmedDB, childID, parentID));
	}

	public boolean isRedundantIsA(String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		logger.info(reasonedDB+" testing redundant isa "+child+" - "+parent);
		for (Link link : reasonedDB.getParents(child)) {
			if (link.getParent().equals(parent) && link.getType().equals(OBOProperty.IS_A)) {
				return ReasonerUtil.isRedundant(reasonedDB, link);
			}
		}
		return false;
	}

	public void testForRedundantIsA(String childID, String parentID)  {
		assertTrue(isRedundantIsA(childID,parentID));
	}
	public void testForNonRedundantIsA(String childID, String parentID)  {
		assertFalse(isRedundantIsA(childID,parentID));
	}



	public void testForLink(String childID, String relID, String parentID)  {
		assertTrue(hasLink(childID, relID, parentID));

	}
	public void testForNoLink(String childID, String relID, String parentID)  {
		assertFalse(hasLink(childID, relID, parentID));
	}

	public void testForLinkInTrimmed(String childID, String relID, String parentID)  {
		assertTrue(hasLink(trimmedDB, childID, relID, parentID));

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

	public String tab(int depth) {
		StringBuffer sb = new StringBuffer();
		for (int i=0; i<depth; i++) {
			sb.append("  ");
		}
		return sb.toString();
	}

	public void showExplanation(Link link, int depth) {
		System.out.println(tab(depth)+"LINK: "+link);
		depth++;
		if (depth > 10) {
			System.err.println("TOO DEEP!");
			return;
		}
		for (Explanation exp : reasonedDB.getExplanations(link)) {
			System.out.println(tab(depth)+"EXP: "+exp);
			for (Link elink : exp.getEvidence()) {
				showExplanation(elink, depth+1);
			}

		}

	}


	public File writeTempTrimmedReasonedOBOFile() throws IOException, DataAdapterException {

		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		OBOSerializationEngine.FilteredPath path = new OBOSerializationEngine.FilteredPath();

		File outFile = File.createTempFile("foo", "bar");
		//outFile.deleteOnExit();
		path.setPath(outFile.getAbsolutePath());
		path.setSaveImplied(true);

		config.getSaveRecords().add(path);
		//config.setWritePath(outFile.getAbsolutePath());
		path.setReasonerFactory(reasonerFactory);
		config.setBasicSave(false);
		logger.info("writePath = " + config.getWritePath());
		logger.info("savePath = " + config.getSaveRecords());

		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		return outFile;
	}


}
