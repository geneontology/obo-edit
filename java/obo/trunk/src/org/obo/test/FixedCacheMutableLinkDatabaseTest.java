package org.obo.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.datamodel.impl.FixedCacheMutableLinkDatabase;
import org.obo.reasoner.ReasonedLinkDatabase;

import org.apache.log4j.*;

public class FixedCacheMutableLinkDatabaseTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FixedCacheMutableLinkDatabaseTest.class);

	protected ReasonedLinkDatabase reasoner;
	
	public FixedCacheMutableLinkDatabaseTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"nucleus.obo"};
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		logger.info("Setting up: " + this);
		super.setUp();
		linkDatabase = new FixedCacheMutableLinkDatabase(session, true);
		//linkDatabase = new DefaultLinkDatabase(session);
		reasonedDB.setLinkDatabase(linkDatabase);
		reasonedDB.recache();

		
	}

	public void testHasLoaded() {
		// TODO
		testForIsA("GO:0043227","GO:0005575");
		assertTrue(true);
	}
	

	public static void addTests(TestSuite suite) {
		suite.addTest(new FixedCacheMutableLinkDatabaseTest("testHasLoaded"));
	}
}



