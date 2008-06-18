package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;

import org.apache.log4j.*;

public class HoldsOverChainReasonerTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HoldsOverChainReasonerTest.class);

	public HoldsOverChainReasonerTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"positive_regulation_of_anti_apoptosis.obo"};
		return Arrays.asList(files);
	}
	
	

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testReasonedLinks() throws Exception {
		String PRoAA = "GO:0045768";
		String APOPTOSIS = "GO:0006915";
		String BCL2 = "MGI:MGI:88139";
		String IR = "indirectly_regulates";
		String INR = "indirectly_negatively_regulates";
		OBOProperty propIR = (OBOProperty) session.getObject(IR);
		for (List<OBOProperty >chain : propIR.getHoldsOverChains()) {
			logger.info(chain);
		}
		this.testForLink(PRoAA, IR, APOPTOSIS);
		this.testForLink(PRoAA, INR, APOPTOSIS);
		for (Link link : reasonedDB.getParents((LinkedObject) session.getObject(BCL2))) {
			logger.info("Bcl2: "+link);
		}
		testForLink(BCL2,"indirect_negative_regulator_of",APOPTOSIS);
	}


}

