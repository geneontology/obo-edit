package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.AnnotationUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;
import java.util.*;

import org.apache.log4j.*;

public class ExtendedInfoComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExtendedInfoComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ReloadListener reloadlistener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			update();
		}
	};

	protected static final int MAX_PARENTAGE = 100;

	protected HashMap scratch = new HashMap();

	public ExtendedInfoComponent(String id) {
		super(id);
	}

	@Override
	public void init() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		attachListeners();
		update();
	}

	@Override
	public String getName() {
		return "Extended Info";
	}

	public void update() {
		removeAll();
		scratch.clear();
		OBOSession session = SessionManager.getManager().getSession();

		JLabel totalTermsLabel = new JLabel("Total terms = 00000000");
		int totalTermCount = 0;
		int definedTermCount = 0;
		totalTermsLabel.setFont(getFont());

		List<JLabel> labels = new Vector<JLabel>();
		labels.add(totalTermsLabel);

		Hashtable catHash = new Hashtable();
		Iterator it = SessionManager.getManager().getSession().getSubsets()
		.iterator();
		while (it.hasNext()) {
			TermSubset ct = (TermSubset) it.next();
			catHash.put(ct, new Integer(0));
		}

		int[] parentCounts = new int[MAX_PARENTAGE];
		Iterator iter = TermUtil.getTerms(
				SessionManager.getManager().getSession()).iterator();
		while (iter.hasNext()) {
			OBOClass term = (OBOClass) iter.next();
			if (term.isBuiltIn() || term.isObsolete())
				continue;
			int count = 0;

			// add one for the current top-level term
			totalTermCount++;

			if (term.getDefinition() != null
					&& term.getDefinition().length() > 0)
				definedTermCount++;

			it = term.getSubsets().iterator();
			while (it.hasNext()) {
				TermSubset tc = (TermSubset) it.next();
				Integer integerCount = (Integer) catHash.get(tc);
				int intCount = integerCount.intValue();
				intCount++;
				catHash.put(tc, new Integer(intCount));
			}

			int parentIndex = term.getParents().size();
			if (parentIndex >= MAX_PARENTAGE)
				parentIndex = MAX_PARENTAGE - 1;
			parentCounts[parentIndex]++;

			count++;
			/*
			 * JLabel childLabel = new JLabel(" "+child+" contains "+count);
			 * childLabel.setFont(getFont()); labels.add(childLabel);
			 */
		}

		Collection<OBOClass> roots = TermUtil.getRoots(SessionManager
				.getManager().getSession());
		for (OBOClass term : roots) {
			if (term.isBuiltIn())
				continue;
			Collection<LinkedObject> s = TermUtil.getDescendants(term);
			JLabel label = new JLabel("   " + term + " (" + term.getID()
					+ ") has " + s.size() + " descendants");
			label.setFont(getFont());
			labels.add(label);
		}

		totalTermsLabel.setText("Total terms = " + totalTermCount);

		if (totalTermCount > 0) {
			int i = MAX_PARENTAGE - 1;
			int acc = 0;
			for (; i >= 0; i--) {
				int currentPercentage = (acc + parentCounts[i]) * 100
				/ totalTermCount;
				if (currentPercentage < 1)
					acc += parentCounts[i];
				else
					break;
			}

			for (int j = 0; j < i + 1; j++) {
				int percent = (100 * parentCounts[j]) / totalTermCount;
				String percentStr;
				if (percent == 0 && parentCounts[j] > 0)
					percentStr = " < 1";
				else
					percentStr = percent + "";
				String str = "   terms with " + j + " parent"
				+ (j == 1 ? "" : "s") + ": " + parentCounts[j] + " ("
				+ percentStr + "%)";
				JLabel label = new JLabel(str);
				label.setFont(getFont());
				labels.add(label);
			}
			if (acc > 0) {
				String str = "   terms with > " + i + " parents: " + acc
				+ " ( < 1%)";
				JLabel label = new JLabel(str);
				label.setFont(getFont());
				labels.add(label);
			}
		}

		Enumeration e = catHash.keys();
		while (e.hasMoreElements()) {
			TermSubset tc = (TermSubset) e.nextElement();
			Integer tcCount = (Integer) catHash.get(tc);
			JLabel catLabel = new JLabel("Subset: " + tc.getDesc() + " has "
					+ tcCount + " members");
			catLabel.setFont(getFont());
			labels.add(catLabel);
		}

		int numInstances = TermUtil.getInstances(session).size();
		if (numInstances>0) {
			JLabel instanceCountLabel = new JLabel(numInstances
					+ " instances");
			instanceCountLabel.setFont(getFont());
			labels.add(instanceCountLabel);
		}

		int numAnnotations = AnnotationUtil.getAnnotations(session).size();
		if (numAnnotations>0) {
			JLabel annotCountLabel = new JLabel(numAnnotations
					+ " annotations");
			annotCountLabel.setFont(getFont());
			labels.add(annotCountLabel);
		}


		//terms with definitions
		int defPercent = (int) (100 * ((double) definedTermCount / (double) totalTermCount));
		JLabel defCountLabel = new JLabel(defPercent
				+ "% of terms have definitions (" + definedTermCount + " of "
				+ totalTermCount + ")");
		defCountLabel.setFont(getFont());
		labels.add(defCountLabel);


		//reasoner status
		String reasonerStatus = SessionManager.getManager().getReasonerName();
		JLabel reasonerStatusLabel = new JLabel("Reasoner: " + reasonerStatus);
		labels.add(reasonerStatusLabel);

		// memory label 
		int memPercent = (int) (100 * ((double) Runtime.getRuntime()
				.freeMemory() / (double) Runtime.getRuntime().totalMemory()));

		JLabel memoryLabel = new JLabel("Memory: " + memPercent + "% of memory free");
		memoryLabel.setFont(getFont());
		labels.add(memoryLabel);

		for (int i = 0; i < labels.size(); i++) {
			add((JLabel) labels.get(i));
		}

		revalidate();

		JDialog dialog = (JDialog) SwingUtilities.getAncestorOfClass(
				JDialog.class, this);
		if (dialog != null) {
			dialog.pack();
		}
	}

	private void attachListeners() {
		GUIUtil.addReloadListener(reloadlistener);
	}

	@Override
	public void cleanup() {
		GUIUtil.removeReloadListener(reloadlistener);
	}
}
