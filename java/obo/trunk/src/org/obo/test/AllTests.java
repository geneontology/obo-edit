package org.obo.test;



import junit.framework.*;


public class AllTests extends TestCase {

	public static Test suite() {
		TestSuite out = new TestSuite();

		out.addTestSuite(IDUpdateTest.class);

		return out;
	}
}

