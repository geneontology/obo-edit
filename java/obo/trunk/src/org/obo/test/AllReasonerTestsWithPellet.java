package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

import org.apache.log4j.*;

public class AllReasonerTestsWithPellet extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllReasonerTestsWithPellet.class);

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new PelletWrappedReasonerFactory());


		return AllReasonerTests.suite();
	}
}
