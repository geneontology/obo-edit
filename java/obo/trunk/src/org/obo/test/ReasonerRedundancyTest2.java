package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.reasoner.impl.LinkPileReasonerFactory;

import org.apache.log4j.*;

public class ReasonerRedundancyTest2 extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerRedundancyTest2.class);

	public ReasonerRedundancyTest2(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"bone.obo"};
		return Arrays.asList(files);
	}
	

	public void testLinks() throws Exception {
		AbstractReasonerTest.setReasonerFactory(new LinkPileReasonerFactory());
		//AbstractReasonerTest.setReasonerFactory(new ForwardChainingReasonerFactory());
		
		testForIsA("endochondral_bone","bone"); /* genus */
		testForIsA("tripus","bone"); /* asserted */
		testForIsA("tripus","endochondral_bone"); /* completeness */

		testForIsAInTrimmed("tripus","endochondral_bone");
		testForRedundantIsA("tripus","bone");
		
	}


}

