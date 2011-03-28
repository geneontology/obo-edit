package org.obo.test;

import java.util.Arrays;
import java.util.Collection;

import org.apache.log4j.Logger;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.impl.DirectLinkReasonerFactory;

public class DirectLinkReasonerTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DirectLinkReasonerTest.class);

	public DirectLinkReasonerTest(String name) {
		super(name);
	}

	public void setUp() throws Exception {
		setReasonerFactory(new DirectLinkReasonerFactory());
		super.setUp();
	}


	public Collection<String> getFilesToLoad() {
		String[] files={"directlinktest.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testSubsumed() throws Exception {
		testForIsA("X:1","X:2");
	}
	public void testAssertedDisjoint() throws Exception {
		hasLink("X:5",OBOProperty.DISJOINT_FROM.getID(),"X:4");
	}
	public void testInferredDisjoint() throws Exception {
		hasLink("X:4",OBOProperty.DISJOINT_FROM.getID(),"X:5");
	}

	public void testAssertedDisjoint2() throws Exception {
		boolean ok = false;	
		for (Link link : reasonedDB.getChildren((LinkedObject) session.getObject("X:4"))) {
			if (link.getType().equals(OBOProperty.DISJOINT_FROM)) {
				if (link.getChild().getID().equals("X:5") && link.getParent().getID().equals("X:4")) {
					ok = true;
				}
			}
		}
		assertTrue(ok);
	}
	public void testInferredDisjoint2() throws Exception {
		boolean ok = false;
		for (Link link : reasonedDB.getChildren((LinkedObject) session.getObject("X:5"))) {
			if (link.getType().equals(OBOProperty.DISJOINT_FROM)) {
				if (link.getChild().getID().equals("X:4") && link.getParent().getID().equals("X:5")) {
					ok = true;
				}

			}
		}
		assertTrue(ok);
	}

	public void testAllLinks() throws Exception {
		IdentifiedObject x = session.getObject("X:5");
		for (Link link : ((LinkedObject) x).getParents()) {
			System.out.println("ALINK:"+link);
		}
		for (Link link : reasonedDB.getParents((LinkedObject) x)) {
			System.out.println("ILINK:"+link);
		}
		testForIsA("X:1","X:2");
	}



}

