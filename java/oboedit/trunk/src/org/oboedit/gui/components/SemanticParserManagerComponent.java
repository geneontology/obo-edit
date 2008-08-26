package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.*;
import org.obo.datamodel.impl.*;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.SemanticParser;
import org.obo.nlp.impl.RegulationTermParser;
import org.obo.reasoner.ReasonerListener;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Collection;

//import java.util.logging.Logger;

import org.apache.log4j.*;

public class SemanticParserManagerComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SemanticParserManagerComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	


//	protected JCheckBox useReasonerCheckbox = new JCheckBox("Use reasoner");
	protected JCheckBox useSemanticParserCheckbox = new JCheckBox("Use semantic parser");
//	protected JCheckBox addSynonymsCheckbox = new JCheckBox("Add synonyms");

	protected JEditorPane summaryField = new JEditorPane();

	protected JScrollPane summaryScroller = new JScrollPane(summaryField,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

// 	protected ReasonerListener reasonerActionListener = new ReasonerListener() {

// 		public void reasoningFinished() {
// 			updateProgressPanel(SessionManager.getManager().getUseReasoner());
// 		}

// 		public void reasoningStarted() {
// 		}
// 	};

// 	protected ActionListener reasonerListener = new ActionListener() {
// 		public void actionPerformed(ActionEvent e) {
// 			enableReasoner(useReasonerCheckbox.isSelected());
// 			// updateProgressPanel(useReasonerCheckbox.isSelected());
// 		}
// 	};

	protected ActionListener semanticParserListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			enableSemanticParser(useSemanticParserCheckbox.isSelected());
			// updateProgressPanel(useReasonerCheckbox.isSelected());
		}
	};

	/*
	 * protected ProgressListener progressListener = new ProgressListener() {
	 * public void progressMade(ProgressEvent e) { if (e instanceof
	 * ReusableProgressEvent) { barUpdater.setEvent((ReusableProgressEvent) e);
	 * SwingUtilities.invokeLater(barUpdater); } } };
	 */

	public SemanticParserManagerComponent(String id) {
		super(id);
		setPreferredSize(new Dimension(250,50));
		setLayout(new BorderLayout());
		summaryField.setPreferredSize(new Dimension(300, 200));
		summaryField.setContentType("text/html");

		setLayout(new BorderLayout());
//		useReasonerCheckbox.setOpaque(false);
		// add(progressBar, "North");
		add(useSemanticParserCheckbox, "North");
//		add(useReasonerCheckbox, "South");

		summaryField.setEditable(false);
	}

 	protected void enableReasoner(final boolean enableReasoner) {
 		SessionManager.getManager().setUseReasoner(enableReasoner);
 	}
	protected void enableSemanticParser(final boolean enableParser) {
		if (enableParser) {
			logger.info("creating parser");
			// TODO - allow others
			SemanticParser sp = new RegulationTermParser();
			sp.index(SessionManager.getManager().getSession());
			Collection<TermMacroHistoryItem> items = sp.parseTerms();
			logger.info("new items: "+items.size());
			sp.apply(items);
			
		}
	}

	protected void updateProgressPanel(final boolean enableReasoner) {

		String text = "<html><body>\n";
		text += "Reasoning completed.";
		text += "</body></html>";
		//logger.info("newLinks = "+reasoner.getNewLinks());
		final String summaryText = text;

		Runnable screenUpdate = new Runnable() {
			public void run() {
				if (enableReasoner)
					add(summaryScroller, "Center");
				else
					remove(summaryScroller);

//				useReasonerCheckbox.removeActionListener(reasonerListener);
//				useReasonerCheckbox.setSelected(enableReasoner);
				
				useSemanticParserCheckbox.addActionListener(semanticParserListener);
				validate();
				repaint();
				summaryField.setText(summaryText);
			}
		};
		SwingUtilities.invokeLater(screenUpdate);
	}
	


	@Override
	public void init() {
		updateProgressPanel(SessionManager.getManager().getUseReasoner());

		
		/*
		 * useReasonerCheckbox.removeActionListener(reasonerListener);
		 * useReasonerCheckbox.setSelected(controller.getUseReasoner()); for(int
		 * i=1; i < 4; i++) tabbedPane.setEnabledAt(i,
		 * controller.getUseReasoner());
		 * useReasonerCheckbox.addActionListener(reasonerListener);
		 */

//		SessionManager.getManager().addReasonerListener(reasonerActionListener, true);

		//updateProgressPanel(SessionManager.getManager().getUseReasoner());
	}

	@Override
	public String getName() {
		return "Semantic Parser Manager";
	}

	@Override
	public void cleanup() {
//		SessionManager.getManager().removeReasonerListener(
//				reasonerActionListener);
	}
}
