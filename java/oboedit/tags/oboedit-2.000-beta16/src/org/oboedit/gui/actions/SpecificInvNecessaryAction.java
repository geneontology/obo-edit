package org.oboedit.gui.actions;


import java.util.*;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import javax.swing.*;

public class SpecificInvNecessaryAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;

	protected boolean changeTo;

	public SpecificInvNecessaryAction(boolean changeTo) {
		this.changeTo = changeTo;
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Change inverse necessity to " + changeTo;
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
		Iterator<Link> it = sources.getLinks().iterator();
		while (it.hasNext()) {
			Link link = it.next();
			if (link instanceof OBORestriction) {
				OBORestriction tr = (OBORestriction) link;
				if (PathUtil.isFake(tr))
					continue;
				if (tr.isInverseNecessarilyTrue() != changeTo) {
					found = true;
					break;
				}
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
		List<HistoryItem> items = new LinkedList<HistoryItem>();

		Iterator it = sources.getLinks().iterator();
		while(it.hasNext()) {
			OBORestriction tr = (OBORestriction) it.next();

			if (tr.isInverseNecessarilyTrue() != changeTo) {
				items.add(new InverseNecHistoryItem(tr));
			}
		}

		HistoryItem item;
		if (items.size() == 1)
			item = (HistoryItem) items.get(0);
		else {
			item = new TermMacroHistoryItem("Changed inverse necessity");
			for (int i = 0; i < items.size(); i++)
				((TermMacroHistoryItem) item)
						.addItem((HistoryItem) items.get(i));
		}

		GUIUtil.setSelections(item, sources, sources);

		return item;
	}
}
