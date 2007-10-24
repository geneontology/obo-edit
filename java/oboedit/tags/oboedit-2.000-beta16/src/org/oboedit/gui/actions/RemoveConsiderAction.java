package org.oboedit.gui.actions;


import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;

public class RemoveConsiderAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;
	protected ObsoletableObject clearObject;
	protected ObsoletableObject considerObject;

	public RemoveConsiderAction() {
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Remove consider term";
	}

	public String getDesc() {
		return getName();
	}

	public List<EditAction> getSubActions() {
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
				|| !link.getType().equals(OBOProperty.CONSIDER)) {
			isLegal = false;
			return;
		}

		clearObject = (ObsoletableObject) link.getParent();
		considerObject = (ObsoletableObject) link.getChild();
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		RemoveConsiderHistoryItem item = new RemoveConsiderHistoryItem(
				clearObject, considerObject);

		Selection out = SelectionManager.createSelectionFromTerms(null,
				Collections.singleton((LinkedObject) clearObject), null, false);
		GUIUtil.setSelections(item, out, out);

		return item;
	}
}
