package org.obo.nlp.test;



import junit.framework.*;


public class AllNLPTests extends TestCase {

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

