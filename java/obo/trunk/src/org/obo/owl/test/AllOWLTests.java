package org.obo.owl.test;



import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


/**
 * Runs entire org.obo.owl test suite.
 * May take some time!
 * @author cjm
 *
 */
public class AllOWLTests extends TestCase {

	protected AllOWLTests(String name) {
		super(name);
	}
	
	public static Test suite() {
		TestSuite out = new TestSuite();

		out.addTestSuite(AnnotationsInOWLTest.class);
		out.addTestSuite(BasicOWLTest.class);
		out.addTestSuite(BioTopRoundtripOWLTest.class);
		out.addTestSuite(NCBOStyleOWLTest.class);
		out.addTestSuite(NIFRoundtripOWLTest.class);
		out.addTestSuite(OBIRoundtripOWLTest.class);
		out.addTestSuite(OWLIndividualTest.class);
		out.addTestSuite(SAORoundtripOWLTest.class);
		out.addTestSuite(SubclassAxiomAnnotationTest.class);
		out.addTestSuite(TransitiveOverOWLTest.class);
		return out;
	}
}

