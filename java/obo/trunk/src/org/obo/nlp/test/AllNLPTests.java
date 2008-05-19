package org.obo.nlp.test;



import junit.framework.*;


import org.apache.log4j.*;

public class AllNLPTests extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllNLPTests.class);

	protected AllNLPTests(String name) {
		super(name);
	}
	
	public static Test suite() {
		TestSuite out = new TestSuite();

		out.addTestSuite(RegulationTermParserTest.class);
		out.addTestSuite(PositiveRegulationTermParserTest.class);
		out.addTestSuite(RegulationOfSomitogenesisTest.class);
		return out;
	}
}

