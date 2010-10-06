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

public class ReasonerEquivalentToTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerEquivalentToTest.class);

	public ReasonerEquivalentToTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"equivtest2.obo"};
		return Arrays.asList(files);
	}

	public void testEquivalence() throws Exception {
		this.testForIsA("ab", "a");
		this.testForIsA("a", "ab");
	}
}

