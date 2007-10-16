package org.oboedit.test;

import org.obo.test.ReasonerTest;

import junit.framework.*;

public class AllTests extends TestCase {

	public static Test suite() {
		TestSuite out = new TestSuite();

		out.addTestSuite(SimpleOBORoundtripTest.class);

		out.addTestSuite(OBO2FlatTest.class);
		out.addTestSuite(OBO2OBOTest.class);

		out.addTestSuite(SimpleSerialRoundtripTest.class);

		out.addTestSuite(DatamodelTest.class);

		out.addTestSuite(AddActionTest.class);
		out.addTestSuite(TermScopeSynonymTest.class);
		out.addTestSuite(TermRemoveSynonymTest.class);
		out.addTestSuite(TermSynonymTest.class);
		out.addTestSuite(TermSecondaryIDTest.class);
		out.addTestSuite(TermRemoveSecondaryIDTest.class);
		out.addTestSuite(TermCommentTest.class);
		out.addTestSuite(TermNamespaceTest.class);
		out.addTestSuite(TermNoNamespaceTest.class);
		out.addTestSuite(TermTextTest.class);
		out.addTestSuite(TermDefTest.class);

		out.addTestSuite(HistoryTest.class);

		out.addTestSuite(ReasonerTest.class);
		return out;
	}
}
