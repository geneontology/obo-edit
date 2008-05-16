package org.oboedit.gui.actions;


import java.util.*;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.tree.TreePath;
import javax.swing.*;

import org.apache.log4j.*;

public class RemoveReplacementAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RemoveReplacementAction.class);

	protected Selection sources;

	protected boolean isLegal = false;
	protected ObsoletableObject clearObject;
	protected ObsoletableObject replacementObject;

	public RemoveReplacementAction() {
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Remove replacement term";
	}

	public String getDesc() {
		return getName();
	}

	public List getSubActions() {
		return null;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = true;
		this.sources = sources;
		if (sources.getLinks().size() != 1) {
			isLegal = false;
			return;
		}

		Link link = sources.getLinkSubSelection();

		if (!TermUtil.isObsolete(link.getParent())
				|| !link.getType().equals(OBOProperty.REPLACES)) {
			isLegal = false;
			return;
		}

		clearObject = (ObsoletableObject) link.getParent();
		replacementObject = (ObsoletableObject) link.getChild();
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		RemoveReplacementHistoryItem item = new RemoveReplacementHistoryItem(
				clearObject, replacementObject);

		Selection out = SelectionManager.createSelectionFromTerms(null,
				Collections.singleton((LinkedObject) clearObject), null, false);
		GUIUtil.setSelections(item, out, out);

		return item;
	}
}
