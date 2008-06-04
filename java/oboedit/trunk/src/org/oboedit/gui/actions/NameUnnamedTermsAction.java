package org.oboedit.gui.actions;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;


import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;

import org.obo.datamodel.Link;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOProperty;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.NamerUtil;
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

import org.apache.log4j.*;

public class NameUnnamedTermsAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameUnnamedTermsAction.class);

	protected Selection sources;

	protected boolean isLegal = false;
	
	
	protected JPanel panel;
	Collection<HistoryItem> nameItems = null;
	
	public NameUnnamedTermsAction() {
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Name unnamed terms...";
	}

	public String getDesc() {
		return "Name unnamed terms...";
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

		setupPanel(panel);

		JPanel outerPanel = new JPanel();
		outerPanel.setLayout(new BorderLayout());
		outerPanel
				.add(
						new JLabel(
								"<html>This will create names for any term lacking a name, based on intersection or union definitions.</html>"),
						"North");
			
		JScrollPane sp = new JScrollPane(panel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		outerPanel.add(sp, "Center");

		SimpleWizard wizard = new SimpleWizard(outerPanel);
		if (!wizard.isCancelled()) {
			TermMacroHistoryItem item = new TermMacroHistoryItem(
					"New names for  "+nameItems.size()+" terms");
			for (HistoryItem ni : nameItems) {
				item.addItem(ni);
			}
			return item;
		}
		return null;
	}
	
	protected void setupPanel(JPanel panel) {
		nameItems = NamerUtil.getNameUnnamedObjectsAction(SessionManager.getManager().getSession());
		panel.add(Box.createVerticalGlue());
		
	}
}
