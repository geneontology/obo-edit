package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.OBOProperty;


import org.apache.log4j.*;

public class SubRelationReasonerTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SubRelationReasonerTest.class);

	public SubRelationReasonerTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"directly_develops_from_test.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testAssertedSubRelation() throws Exception {
		testForIsA("directly_develops_from", "develops_from");
		testForIsAInTrimmed("directly_develops_from", "develops_from");
	}
	public void testAssertedLink() throws Exception {
		testForLink("a", "directly_develops_from", "b");
		testForLink("b", "directly_develops_from", "c");
	}
	public void testInferredLinkBasic() throws Exception {
		testForLink("a", "develops_from", "b");
	}
	public void testInferredLink() throws Exception {
		testForLink("a", "develops_from", "c");
	}
	public void testNegativeInferredLink() throws Exception {
		testForNoLink("a", "directly_develops_from", "c");
	}
	public void testNegativeInferredXP() throws Exception {
		testForNoIsA("a", "DIRECTLY_Develops_from_C");
	}

	public void testInferredBasic() throws Exception {
		testForIsA("DIRECTLY_Develops_from_C","Develops_from_C");
		testForIsAInTrimmed("DIRECTLY_Develops_from_C","Develops_from_C");
	}
	public void testInferredXP() throws Exception {
		testForIsA("b","DIRECTLY_Develops_from_C"); 
		testForIsAInTrimmed("b","DIRECTLY_Develops_from_C"); 
		testForIsA("b","Develops_from_C"); 
		testForNoIsAInTrimmed("b","Develops_from_C");
		testForIsA("a","Develops_from_C"); 
		testForIsAInTrimmed("a","Develops_from_C"); 
		//testForNoIsAInTrimmed("b","Develops_from_C");  // redundant
		//testForIsAInTrimmed("b","directly_develops_from_c"); //  in trimmed set
		//testForIsAInTrimmed("b","develops_from_c"); 
	}
	public void testTrim() throws Exception {
		
	}
	public void testMetadata() throws Exception {
		OBOProperty p = (OBOProperty) session.getObject("metadata_test_relation");
		assertTrue(p.isMetadataTag());
		assertTrue(p.isNonInheritable());
		
	}


}

