package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.ForwardChainingReasonerFactory;

import org.apache.log4j.*;

public class AllReasonerTestsWithFCR extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllReasonerTestsWithFCR.class);

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new ForwardChainingReasonerFactory());


		return AllReasonerTests.suite();
	}
}
