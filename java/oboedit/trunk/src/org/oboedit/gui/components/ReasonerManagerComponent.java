package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.util.*;
import org.obo.datamodel.impl.*;
import org.obo.reasoner.ReasonerListener;
import org.obo.reasoner.ReasonerRegistry;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import org.apache.log4j.*;

public class ReasonerManagerComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerManagerComponent.class);

    private static SessionManager sessionManager = SessionManager.getManager();
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JComboBox reasonerChoice = new JComboBox();

	protected JEditorPane summaryField = new JEditorPane();

	protected JScrollPane summaryScroller = new JScrollPane(summaryField,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        protected ReasonerRegistry registry = ReasonerRegistry.getInstance();

	protected ReasonerListener reasonerActionListener = new ReasonerListener() {

		public void reasoningFinished() {
			updateProgressPanel(sessionManager.getUseReasoner());
		}

		public void reasoningStarted() {
		    logger.debug("ReasonerManagerComponent: reasoningStarted"); // DEL
		}
	};

	protected ActionListener reasonerListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    logger.debug("actionPerformed: calling enableReasoner " + reasonerChoice.getSelectedItem()); // DEL
		    enableReasoner((String)reasonerChoice.getSelectedItem());
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
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());	
		setPreferredSize(new Dimension(450,100));
		summaryField.setPreferredSize(new Dimension(400, 25));
		summaryField.setContentType("text/html");
		summaryField.setEditable(false);

		add(new JLabel("Reasoner"),BorderLayout.WEST);
		add(reasonerChoice,BorderLayout.EAST);
		add(summaryField,BorderLayout.SOUTH);
		
		reasonerChoice.addItem("OFF");
		// Get reasoner names from registry
		ReasonerRegistry registry = ReasonerRegistry.getInstance();
		logger.debug("Registered reasoners: " + registry.getRegisteredNames()); // DEL
		for (String registryName : registry.getRegisteredNames()) 
		    reasonerChoice.addItem(registryName);

		reasonerChoice.setMaximumSize(
		    new Dimension(Integer.MAX_VALUE, reasonerChoice.getPreferredSize().height));

		reasonerChoice.setSelectedItem(sessionManager.getReasonerName());
		
	}

	protected void enableReasoner(String reasonerChoice) {
	    summaryField.setText("");
	    sessionManager.setReasonerName(reasonerChoice);
	}

	protected void updateProgressPanel(final boolean enableReasoner) {
		String text = "";
		if (enableReasoner) {
			text += "<html><body>\n";
			text += "Reasoning completed.";
			text += "</body></html>";
		}
		final String summaryText = text;

		Runnable screenUpdate = new Runnable() {
			public void run() {
			    summaryField.setText("");
				if (enableReasoner)
					add(summaryScroller, "South");
				else
					remove(summaryScroller);

				reasonerChoice.removeActionListener(reasonerListener);
				reasonerChoice.setSelectedItem(sessionManager.getReasonerName());
				reasonerChoice.addActionListener(reasonerListener);

				validate();
				repaint();
				summaryField.setText(summaryText);
			}
		};
		SwingUtilities.invokeLater(screenUpdate);
	}

	@Override
	public void init() {
		sessionManager.addReasonerListener(reasonerActionListener, true);

		updateProgressPanel(sessionManager.getUseReasoner());
	}

	@Override
	public String getName() {
		return "Reasoner Manager";
	}

	@Override
	public void cleanup() {
		sessionManager.removeReasonerListener(
				reasonerActionListener);
	}
}
