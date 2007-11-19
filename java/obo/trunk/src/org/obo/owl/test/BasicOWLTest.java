package org.obo.owl.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;

public class BasicOWLTest extends AbstractOWLTest {

	protected BasicOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "SAO.owl" };
		return Arrays.asList(files);
	}
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new BasicOWLTest("testHasLoaded"));
	}
	

}



