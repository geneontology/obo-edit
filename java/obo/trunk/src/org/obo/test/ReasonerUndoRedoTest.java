package org.obo.test;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.PelletWrappedReasonerFactory;
import org.obo.reasoner.impl.ReasonerOperationModel;
import org.obo.reasoner.impl.TransitivityExplanation;

public class ReasonerUndoRedoTest extends AbstractReasonerTest {

	public ReasonerUndoRedoTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "gene_ontology_edit.obo" };
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		ReasonerOperationModel reasonerOpModel = new ReasonerOperationModel(
				reasonedDB);
		reasonerOpModel.setSession(session);
		session.getOperationModel().addLockstepModel(reasonerOpModel);
		Collection<Link> allLinks = new ArrayList<Link>();
		for(IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				for(Link link : ((LinkedObject) io).getParents()) {
					allLinks.add(link);
				}
			}
		}
		for(Link link : allLinks) {
			HistoryItem item = new DeleteLinkHistoryItem(link);
			session.getOperationModel().apply(item);
			session.getOperationModel().reverse(item);
			session.getOperationModel().apply(item);
			session.getOperationModel().reverse(item);
		}
	}

	protected void checkForDeadExps() {
		if (reasonedDB instanceof ForwardChainingReasoner) {
			ForwardChainingReasoner reasoner = (ForwardChainingReasoner) reasonedDB;
			checkForDeadExps(reasoner.getExplanationMap().values());
			checkForDeadExps(reasoner.getExplanationDepsMap().values());
		}
	}
	
	protected void checkForDeadExps(Collection<Collection<Explanation>> expsC) {
		for (Collection<Explanation> exps : expsC) {
			for(Explanation exp: exps) {
				if (exp instanceof TransitivityExplanation) {
					TransitivityExplanation t = (TransitivityExplanation) exp;
					assertTrue(t.getDirectLink() != null);
					assertTrue(t.getExtensionLink() != null);
				}
			}
		}
	}

	public static Test suite() {
		setReasonerFactory(new ForwardChainingReasonerFactory());
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new ReasonerUndoRedoTest("testLinks"));
	}
}
