package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.LinkPileReasonerFactory;

import org.apache.log4j.*;

public class AllReasonerTestsWithLPR extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllReasonerTestsWithLPR.class);

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new LinkPileReasonerFactory());

		return AllReasonerTests.suite();

	}
}
