package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

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
import org.obo.datamodel.*;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsImpliedLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;

import org.apache.log4j.*;

public class PropertyIntersectionTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PropertyIntersectionTest.class);

	public PropertyIntersectionTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"relation_intersection.obo"};
		return Arrays.asList(files);
	}



	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testReasonedLinks() throws Exception {
		testForLink("gastrulation","during","lifecycle"); 
	}
}

