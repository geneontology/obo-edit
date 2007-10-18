package org.oboedit.gui.actions;


import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;

public class RerootAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Reroot";
	}

	public String getDesc() {
		return "Reroot";
	}

	public List<EditAction> getSubActions() {
		return null;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = true;
		this.sources = sources;
		if (sources.getTermSubSelection() == null) {
			isLegal = false;
			return;
		}

		LinkedObject term = sources.getTermSubSelection();

		if (RootAlgorithm.STRICT.isRoot(term)) {
			isLegal = false;
			return;
		}
		if (term.getParents().size() != 1) {
			isLegal = false;
			return;
		}
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		LinkedObject term = sources.getTermSubSelection();

		Link tr = term.getParents().iterator().next();

		DeleteLinkHistoryItem item = new DeleteLinkHistoryItem(tr);

		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromTerms(null, Collections.singleton(term),
						term, false));

		return item;
	}
}
