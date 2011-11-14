package org.oboedit.gui.actions;


import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;

public class MakeSubclassesMutuallyDisjointAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Make is_a children disjoint";
	}

	public String getDesc() {
		return "Making is_a children disjoint";
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

		OBOObject term = (OBOObject) sources.getTermSubSelection();

		// TODO: test if legal
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		OBOObject term = (OBOObject) sources.getTermSubSelection();
		TermMacroHistoryItem item = TermUtil.makeAllSubclassesMutuallyDisjointHistoryItem(term);

		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromTerms(null, Collections.singleton(term),
						term, false));

		return item;
	}
}
