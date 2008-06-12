package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;

import org.apache.log4j.*;

public class IntersectionUsingSubRelationsTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionUsingSubRelationsTest.class);

	public IntersectionUsingSubRelationsTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"regulation.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		// inhibition of neurotransmitter uptake IS_A negative regulation of neurotransmitter uptake
		testForIsA("GO:0051609","GO:0051581"); /* completeness */

		testForLink("GO:0051609", "negatively_regulates", "GO:0001504"); /* given */
		testForLink("GO:0051609", "regulates", "GO:0001504"); /* sub-relation */
		
		testForLink("GO:0051609", "regulates", "GO:0008150"); /* sub-rel + IS_A */

		// neg reg of x IS_A reg of x
		testForIsA("GO:0051581","GO:0051580"); /* xp def differs only by relation */
	}

	public static Test suite() {
		logger.info("foo");
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new IntersectionUsingSubRelationsTest("testLinks"));
	}

}

