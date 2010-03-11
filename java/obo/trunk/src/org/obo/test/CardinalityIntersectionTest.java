package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBORestriction;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class CardinalityIntersectionTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CardinalityIntersectionTest.class);

	public CardinalityIntersectionTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "cardinality.obo"};
		return Arrays.asList(files);
	}
	
	public void testForCard() throws Exception {

		OBOObject obj = (OBOObject) session.getObject("human_body");
		int oks = 0;
		for (Link link : obj.getParents()) {
			OBORestriction r = (OBORestriction)link;
			logger.info(r);
			Integer card = r.getCardinality();
			logger.info("c="+card);
			if(r.getParent().getID().equals("leg") && card == 2) {
				oks++;
			}
			if(r.getParent().getID().equals("human_limb") && card == 4) {
				oks++;
			}
		}
		assertTrue(oks == 2);
		writeTempOBOFile();
	}
}
