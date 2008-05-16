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

public class DomainChangeAction implements DropMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DomainChangeAction.class);

	protected boolean fromDrop = false;

	protected boolean isLegal = false;

	protected OBOProperty target;

	protected OBOClass domain;

	protected Selection sources;

	protected GestureTarget dest;

	public String getName() {
		return "Change domain";
	}

	public String getDesc() {
		return "Change domain";
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

		domain = (OBOClass) selection.getTermSubSelection();
		target = (OBOProperty) destItem.getTerm();

		if (ObjectUtil.equals(target.getDomain(), domain)) {
			isLegal = false;
			return;
		}

		isLegal = true;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		DomainHistoryItem item = new DomainHistoryItem(target, domain);
		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromTarget(dest));
		return item;
	}
	
	public KeyStroke getKeyStroke() {
		return null;
	}
}
