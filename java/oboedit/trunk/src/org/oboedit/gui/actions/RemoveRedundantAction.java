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
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.Link;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.EditAction;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Selection;
import org.oboedit.gui.SimpleWizard;

import org.apache.log4j.*;

public class RemoveRedundantAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RemoveRedundantAction.class);

	protected Selection sources;

	protected boolean isLegal = false;

	public RemoveRedundantAction() {
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Remove redundant links...";
	}

	public String getDesc() {
		return "Remove redundant links...";
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
		Iterator<Link> it = TermUtil.getAllLinks(SessionManager.getManager()
				.getSession().getLinkDatabase());
		final Collection<Link> redundantLinks = new LinkedHashSet<Link>();
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		while (it.hasNext()) {
			final Link link = it.next();
			logger.info("examining "+link);
			if (!TermUtil.isImplied(link)) {
				ReasonedLinkDatabase reasoner = SessionManager.getManager()
						.getReasoner();
				Collection<Explanation> explanations = reasoner
						.getExplanations(link);
				for (Explanation explantion : explanations) {
					if (explantion.getExplanationType().equals(
							ExplanationType.TRANSITIVITY)) {
						redundantLinks.add(link);
						final JCheckBox checkBox = new JCheckBox("Delete "
								+ link, true);
						checkBox.addActionListener(new ActionListener() {

							public void actionPerformed(ActionEvent e) {
								if (checkBox.isSelected())
									redundantLinks.add(link);
								else
									redundantLinks.remove(link);
							}
						});
						panel.add(checkBox);
					}
				}
			}
		}
		if (redundantLinks.size() == 0) {
			JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
					"There are no redundant links in the current ontology.");
			return null;
		}
		panel.add(Box.createVerticalGlue());

		JPanel outerPanel = new JPanel();
		outerPanel.setLayout(new BorderLayout());
		outerPanel
				.add(
						new JLabel(
								"<html>The links below are redundant and should be"
										+ "removed. However, you may decide it is necessary to keep some redundant "
										+ "links in your ontology. Only selected links will be deleted.</html>"),
						"North");
		outerPanel.add(Box.createHorizontalStrut(20), "West");
		outerPanel.add(panel, "Center");

		SimpleWizard wizard = new SimpleWizard(outerPanel);
		if (!wizard.isCancelled()) {
			TermMacroHistoryItem item = new TermMacroHistoryItem(
					"Delete "+redundantLinks.size()+" redundant links");
			for (Link link : redundantLinks) {
				item.addItem(new DeleteLinkHistoryItem(link));
			}
			return item;
		}
		return null;
	}
}
