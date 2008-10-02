package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.rbr.RuleBasedReasonerFactory;

import org.apache.log4j.*;

public class AllReasonerTestsWithRBR extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllReasonerTestsWithRBR.class);

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new RuleBasedReasonerFactory());

		return AllReasonerTests.suite();
	}
}
