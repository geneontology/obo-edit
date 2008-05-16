package org.oboedit.gui.actions;

import org.bbop.util.*;

import java.util.*;

import javax.swing.KeyStroke;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import org.apache.log4j.*;

public class RangeChangeAction implements DropMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RangeChangeAction.class);

	protected boolean fromDrop = false;
	protected boolean isLegal = false;

	protected OBOProperty target;
	protected OBOClass range;

	protected Selection sources;
	protected GestureTarget dest;

	public String getName() {
		return "Change range";
	}

	public String getDesc() {
		return "Change range";
	}

	public List getSubActions() {
		return null;
	}

	public void dropInit(Selection selection, GestureTarget destItem) {
		dest = destItem;
		sources = selection;
		if (destItem.getTerm() == null
				|| !TermUtil.isProperty(destItem.getTerm())) {
			isLegal = false;
			return;
		}

		if (selection.getTerms().size() != 1
				|| !TermUtil.isClass(selection.getTermSubSelection())) {
			isLegal = false;
			return;
		}

		range = (OBOClass) selection.getTermSubSelection();
		target = (OBOProperty) destItem.getTerm();

		if (ObjectUtil.equals(target.getDomain(), range)) {
			isLegal = false;
			return;
		}

		isLegal = true;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		RangeHistoryItem item = new RangeHistoryItem(target, range);
		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromTarget(dest));
		return item;
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

}
