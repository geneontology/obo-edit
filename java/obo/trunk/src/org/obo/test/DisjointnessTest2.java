package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class DisjointnessTest2 extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DisjointnessTest2.class);

	public DisjointnessTest2(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "regulation_of_somitogenesis.obo"};
		return Arrays.asList(files);
	}
	

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}


	public void testMakeDisj() {
		OBOObject obj = (OBOObject) session.getObject("GO:0008150");
		TermMacroHistoryItem item = TermUtil.makeAllSubclassesMutuallyDisjointHistoryItem(obj);

		DefaultOperationModel model = new DefaultOperationModel();
		model.setSession(session);
		model.apply(item);

		testForLink("GO:0032501",OBOProperty.DISJOINT_FROM.getID(),"GO:0065007");

	}
}


