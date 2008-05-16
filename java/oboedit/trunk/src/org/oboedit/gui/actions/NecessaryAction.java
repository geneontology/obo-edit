package org.oboedit.gui.actions;

import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.oboedit.gui.*;

import javax.swing.tree.TreePath;
import javax.swing.*;

import org.apache.log4j.*;

public class NecessaryAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NecessaryAction.class);

	protected Selection sources;

	protected boolean isLegal = false;

	protected List subActions = new Vector();

	public NecessaryAction() {
		subActions.add(new SpecificNecessaryAction(true));
		subActions.add(new SpecificNecessaryAction(false));
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Change necessity";
	}

	public String getDesc() {
		return "Change necessity";
	}

	public List getSubActions() {
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
