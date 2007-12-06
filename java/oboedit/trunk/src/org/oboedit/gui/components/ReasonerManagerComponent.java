package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.*;
import org.obo.datamodel.impl.*;
import org.obo.reasoner.ReasonerListener;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ReasonerManagerComponent extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JCheckBox useReasonerCheckbox = new JCheckBox("Use reasoner");

	protected JEditorPane summaryField = new JEditorPane();

	protected JScrollPane summaryScroller = new JScrollPane(summaryField,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected ReasonerListener reasonerActionListener = new ReasonerListener() {

		public void reasoningFinished() {
			updateProgressPanel(SessionManager.getManager().getUseReasoner());
		}

		public void reasoningStarted() {
		}
	};

	protected ActionListener reasonerListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			enableReasoner(useReasonerCheckbox.isSelected());
			// updateProgressPanel(useReasonerCheckbox.isSelected());
		}
	};

	/*
	 * protected ProgressListener progressListener = new ProgressListener() {
	 * public void progressMade(ProgressEvent e) { if (e instanceof
	 * ReusableProgressEvent) { barUpdater.setEvent((ReusableProgressEvent) e);
	 * SwingUtilities.invokeLater(barUpdater); } } };
	 */

	public ReasonerManagerComponent(String id) {
		super(id);
		setPreferredSize(new Dimension(100,50));
		setLayout(new BorderLayout());
		summaryField.setPreferredSize(new Dimension(300, 200));
		summaryField.setContentType("text/html");

		setLayout(new BorderLayout());
		useReasonerCheckbox.setOpaque(false);
		// add(progressBar, "North");
		add(useReasonerCheckbox, "South");

		summaryField.setEditable(false);
	}

	protected void enableReasoner(final boolean enableReasoner) {
		SessionManager.getManager().setUseReasoner(enableReasoner);
	}

	protected void updateProgressPanel(final boolean enableReasoner) {

		String text = "<html><body>\n";
		text += "Reasoning completed.";
		text += "</body></html>";
		// System.err.println("newLinks = "+reasoner.getNewLinks());
		final String summaryText = text;

		Runnable screenUpdate = new Runnable() {
			public void run() {
				if (enableReasoner)
					add(summaryScroller, "Center");
				else
					remove(summaryScroller);

				useReasonerCheckbox.removeActionListener(reasonerListener);
				useReasonerCheckbox.setSelected(enableReasoner);
				useReasonerCheckbox.addActionListener(reasonerListener);

				validate();
				repaint();
				summaryField.setText(summaryText);
			}
		};
		SwingUtilities.invokeLater(screenUpdate);
	}

	@Override
	public void init() {
		/*
		 * useReasonerCheckbox.removeActionListener(reasonerListener);
		 * useReasonerCheckbox.setSelected(controller.getUseReasoner()); for(int
		 * i=1; i < 4; i++) tabbedPane.setEnabledAt(i,
		 * controller.getUseReasoner());
		 * useReasonerCheckbox.addActionListener(reasonerListener);
		 */

		SessionManager.getManager().addReasonerListener(reasonerActionListener, true);

		updateProgressPanel(SessionManager.getManager().getUseReasoner());
	}

	@Override
	public String getName() {
		return "Reasoner Plugin";
	}

	@Override
	public void cleanup() {
		SessionManager.getManager().removeReasonerListener(
				reasonerActionListener);
	}
}
