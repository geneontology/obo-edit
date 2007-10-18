package org.oboedit.gui.actions;


import java.util.*;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import javax.swing.*;

public class SpecificNecessaryAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;

	protected boolean changeTo;

	public SpecificNecessaryAction(boolean changeTo) {
		this.changeTo = changeTo;
	}
	
	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Change necessity to " + changeTo;
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
		if (sources.getLinks().size() < 1) {
			isLegal = false;
			return;
		}
		boolean found = false;
		Iterator it = sources.getLinks().iterator();
		while(it.hasNext()) {
			OBORestriction tr = (OBORestriction) it.next();
			if (PathUtil.isFake(tr))
				continue;
			if (tr.isNecessarilyTrue() != changeTo) {
				found = true;
				break;
			}
		}
		if (!found) {
			isLegal = false;
			return;
		}
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		Vector<HistoryItem> items = new Vector<HistoryItem>();

		Iterator it = sources.getLinks().iterator();
		while(it.hasNext()) {
			OBORestriction tr = (OBORestriction) it.next();

			if (tr.isInverseNecessarilyTrue() != changeTo) {
				items.add(new NecessarilyTrueHistoryItem(tr));
			}
		}

		HistoryItem item;
		if (items.size() == 1)
			item = (HistoryItem) items.get(0);
		else {
			item = new TermMacroHistoryItem("Changed necessity");
			for (int i = 0; i < items.size(); i++)
				((TermMacroHistoryItem) item)
						.addItem((HistoryItem) items.get(i));
		}

		GUIUtil.setSelections(item, sources, sources);

		return item;
	}
}
