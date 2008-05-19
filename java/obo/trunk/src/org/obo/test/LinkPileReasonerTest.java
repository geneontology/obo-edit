package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.LinkPileReasoner;

import org.apache.log4j.*;

public class LinkPileReasonerTest extends AbstractReasonerFaceoffTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkPileReasonerTest.class);

	protected LinkPileReasonerTest(String name) {
		super(name);
	}

	@Override
	protected void installTestReasoners() {
		setReferenceReasoner(new ForwardChainingReasoner());
		addReasoner(new LinkPileReasoner());
	}

	@Override
	public Collection<String> getFilesToLoad() {
			String[] files = { "so-xp.obo" };
			return Arrays.asList(files);
	}
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		suite.addTest(new LinkPileReasonerTest("testLinks"));
		return suite;
	}
}
