package org.oboedit.gui.actions;


import java.util.*;

import javax.swing.KeyStroke;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import org.apache.log4j.*;

public class AddReplacementAction implements DropMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddReplacementAction.class);

	protected boolean isLegal = false;

	protected ObsoletableObject target;
	protected Selection selection;
	protected GestureTarget destItem;
	protected Collection<ObsoletableObject> replacementTerms = new LinkedList<ObsoletableObject>();

	public String getName() {
		return "Add replacement term";
	}

	public String getDesc() {
		return "Add replacement term";
	}

	public List getSubActions() {
		return null;
	}

	public void dropInit(Selection selection, GestureTarget destItem) {
		replacementTerms.clear();
		this.selection = selection;
		
		if (selection.isEmpty()) {
			isLegal = false;
			return;
		}
		this.destItem = destItem;
		LinkedObject lo = destItem.getTerm();
		if (lo != null && lo instanceof ObsoletableObject) {
			// The target must be obsolete to show this menu item
			if (!TermUtil.isObsolete(lo)) {
				isLegal = false;
				return;
			}
			target = (ObsoletableObject) lo;
		}
		else {
			isLegal = false;
			return;
		}
		
		// If ANY TERM in the selection is the same as the gesture target, isLegal should be false.
		isLegal = false;
		for (LinkedObject term : selection.getTerms()) {
			if (term instanceof ObsoletableObject) {
				if (term.getName().equals(lo.getName())) {
					isLegal = false;
					return;
				}
				isLegal = true;  // found one that's different from the gesture target
				ObsoletableObject consider = (ObsoletableObject) term;
				if (!target.getConsiderReplacements().contains(consider))
					replacementTerms.add(consider);
			}
		}

		if (isLegal == false)
			return;

		if (replacementTerms.size() < 1) {
			isLegal = false;
			return;
		}

		isLegal = true;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		TermMacroHistoryItem item = new TermMacroHistoryItem(
				"add obsolete replacement term");

		Iterator it = replacementTerms.iterator();
		while (it.hasNext()) {
			ObsoletableObject replacementTerm = (ObsoletableObject) it.next();
			AddReplacementHistoryItem addItem = new AddReplacementHistoryItem(
					target, replacementTerm);
			item.addItem(addItem);
		}

		GUIUtil.setSelections(item, selection, SelectionManager
				.createSelectionFromTarget(destItem));

		return item;
	}

	public KeyStroke getKeyStroke() {
		return null;
	}
}
