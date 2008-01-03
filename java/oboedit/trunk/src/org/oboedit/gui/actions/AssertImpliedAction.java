package org.oboedit.gui.actions;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.Link;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOProperty;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.EditAction;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Selection;
import org.oboedit.gui.SimpleWizard;
import java.util.logging.Logger;

public class AssertImpliedAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;
	
	
	protected JComboBox relationChooser;
	protected JPanel panel;
	protected OBOProperty selectedRelation = null;
	protected Collection<Link> impliedLinks = null;

	public AssertImpliedAction() {
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Assert implied links...";
	}

	public String getDesc() {
		return "Assert implied links...";
	}

	public List<EditAction> getSubActions() {
		return null;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = SessionManager.getManager().getUseReasoner();
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		relationChooser = new JComboBox();
		relationChooser.addItem(SessionManager.getManager().getSession().getObject("OBO_REL:is_a"));
		for (OBOProperty p : 
			TermUtil.getRelationshipTypes(SessionManager.getManager().getSession())) {
			if (!p.isBuiltIn()) {
				relationChooser.addItem(p);
			}
		}
		if (selectedRelation == null) {
			selectedRelation = 
				(OBOProperty) SessionManager.getManager().getSession().getObject("OBO_REL:is_a");
		}
		relationChooser.setSelectedItem(selectedRelation);
		relationChooser.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				System.out.println("action="+relationChooser.getSelectedItem());
				selectedRelation=(OBOProperty) relationChooser.getSelectedItem();
				panel.removeAll();
				setupPanel(panel);
				panel.updateUI();
			}
		});

		setupPanel(panel);

		JPanel outerPanel = new JPanel();
		outerPanel.setLayout(new BorderLayout());
		outerPanel
				.add(
						new JLabel(
								"<html>The links below are implied and should be"
										+ "asserted (unless implied links are to be asserted at the time of ontology publishing). "
										+ "Only selected links will be asserted.</html>"),
						"North");
		JPanel chooserPanel = new JPanel();
		chooserPanel.setLayout(new BoxLayout(chooserPanel,BoxLayout.Y_AXIS));
		chooserPanel.add(new JLabel("<html>show relations of type:<br></html>"));
		chooserPanel.add(relationChooser);
		chooserPanel.add(Box.createVerticalGlue());
		outerPanel.add(chooserPanel,"West");
		outerPanel.add(Box.createHorizontalStrut(20), "East");
		
		JScrollPane sp = new JScrollPane(panel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		outerPanel.add(sp, "Center");

		SimpleWizard wizard = new SimpleWizard(outerPanel);
		if (!wizard.isCancelled()) {
			TermMacroHistoryItem item = new TermMacroHistoryItem(
					"Assert "+impliedLinks.size()+" implied links");
			for (Link link : impliedLinks) {
				item.addItem(new CreateLinkHistoryItem(link));
			}
			return item;
		}
		return null;
	}
	
	protected void setupPanel(JPanel panel) {
		ReasonedLinkDatabase reasoner = SessionManager.getManager().getReasoner();
		Iterator<Link> it = TermUtil.getAllLinks(reasoner);
		impliedLinks = new LinkedHashSet<Link>();
		int count = 0;
		Logger logger = Logger.getLogger("org.oboedit.gui");
		logger.info("iterating through all links");
		while (it.hasNext()) {
			final Link link = it.next();
			if (TermUtil.isImplied(link)) {
				logger.info("implied link: "+link);
				Namespace subjNS = link.getChild().getNamespace();
				Namespace objNS = link.getParent().getNamespace();
				//System.err.println("ns: "+subjNS + " " +objNS);
				if (!subjNS.equals(objNS)) {
					// TODO: configurable?
					//System.err.println("ignoring "+link+" as it spans ontologies");
					continue;
				}
				logger.info("checking if this should be trimmed");
				if (!ReasonerUtil.shouldBeTrimmed(reasoner, link) &&
					!impliedLinks.contains(link) &&
					(selectedRelation == null ||
							link.getType().equals(selectedRelation))) {
					logger.info("checking explanations");
					Collection<Explanation> explanations = reasoner.getExplanations(link);
					
					Explanation chosenExplanation = null;
					for (Explanation explanation : explanations) {
						if (!explanation.getExplanationType().equals(
								ExplanationType.TRANSITIVITY)) { 
							chosenExplanation = explanation;
						}
					}
					if (chosenExplanation != null) {

						logger.info("ADDING LINK. chosen explanation: "+chosenExplanation);

						count++;
						final JCheckBox checkBox = new JCheckBox("Assert: ["
								+ link+"]    (EXPLANATION: "+chosenExplanation+")", false);
						checkBox.addActionListener(new ActionListener() {

							public void actionPerformed(ActionEvent e) {
								if (checkBox.isSelected())
									impliedLinks.add(link);
								else
									impliedLinks.remove(link);
							}
						});
						panel.add(checkBox);
					}
				}
			}
		}
		if (count == 0) {
//			JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
//					"There are no implied links in the current ontology.");
//			return null;
		}
		panel.add(Box.createVerticalGlue());
		
	}
}
