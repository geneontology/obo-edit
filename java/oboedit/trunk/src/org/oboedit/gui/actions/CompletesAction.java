package org.oboedit.gui.actions;

import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.oboedit.gui.*;

import javax.swing.*;

import org.apache.log4j.*;

public class CompletesAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CompletesAction.class);

	protected Selection sources;

	protected boolean isLegal = false;

	protected List<EditAction> subActions = new Vector<EditAction>();

	public CompletesAction() {
		subActions.add(new SpecificCompletesAction(true));
		subActions.add(new SpecificCompletesAction(false));
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Change \"is intersection\" status";
	}

	public String getDesc() {
		return getName();
	}

	public List<EditAction> getSubActions() {
		return subActions;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = true;
		this.sources = sources;
		if (sources.getLinks().size() < 1) {
			isLegal = false;
			return;
		}
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		return null;
	}
}
