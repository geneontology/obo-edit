package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.reasoner.rbr.RelationCompositionTable;
import org.obo.reasoner.rbr.RuleBasedReasoner;
import org.obo.util.TermUtil;
import org.obo.datamodel.*;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsImpliedLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;

import org.apache.log4j.*;

public class AdvancedReasonerTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AdvancedReasonerTest.class);

	public AdvancedReasonerTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"genome_intervals.obo"};
		return Arrays.asList(files);
	}

	public void testReasonedLinks() throws Exception {
		OBOProperty df = (OBOProperty) session.getObject("disconnected_from");
		if (reasonedDB instanceof RuleBasedReasoner) {
			RuleBasedReasoner rbr = (RuleBasedReasoner) reasonedDB;
			RelationCompositionTable rct = rbr.getRelationCompositionTable();
			Set<OBOProperty> cset = rct.lookup(df, OBOProperty.IS_A);
			System.out.println(cset);
			assertTrue(cset.isEmpty());
			//System.out.println(rct.toTable());
		}
		this.testForNoLink("SO:0000201", "disconnected_from", "SO:0000001");
		OBOObject ie = (OBOObject) session.getObject("SO:0000201");
		for (Link link : reasonedDB.getParents(ie)) {
			System.out.println(link);
		}
		writeTempTrimmedReasonedOBOFile();
	}
}

