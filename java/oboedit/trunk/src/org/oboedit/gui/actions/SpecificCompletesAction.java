package org.oboedit.gui.actions;


import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;
import javax.swing.*;

import org.apache.log4j.*;

public class SpecificCompletesAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SpecificCompletesAction.class);

	protected Selection sources;

	protected boolean isLegal = false;

	protected boolean changeTo;

	public SpecificCompletesAction(boolean changeTo) {
		this.changeTo = changeTo;
	}
	
	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Change \"is intersection\" to " + changeTo;
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
		for(Link tr : sources.getLinks()) {
			if (PathUtil.isFake(tr))
				continue;
			if (TermUtil.isIntersection(tr) != changeTo) {
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
		List<HistoryItem> items = new LinkedList<HistoryItem>();
		
		for(Link link : sources.getLinks()){
			OBORestriction tr = (OBORestriction) link;
			
			if (tr.getCompletes() != changeTo) {
				items.add(new CompletesHistoryItem(tr));
			}
		}

		HistoryItem item;
		if (items.size() == 1)
			item = items.get(0);
		else {
			item = new TermMacroHistoryItem("Changed intersection status");
			for (int i = 0; i < items.size(); i++)
				((TermMacroHistoryItem) item)
						.addItem(items.get(i));
		}
		
		GUIUtil.setSelections(item, sources, sources);

		return item;
	}
}
