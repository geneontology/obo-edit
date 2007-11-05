package org.oboedit.gui.actions;


import java.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.tree.TreePath;
import javax.swing.*;

public class SpecificTypeChangeAction implements ClickMenuAction {

	protected OBOProperty type;

	protected Selection sources;

	protected boolean isLegal = false;

	public SpecificTypeChangeAction(OBOProperty type) {
		this.type = type;
	}

	public OBOProperty getType() {
		return type;
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return type.toString();
	}

	public String getDesc() {
		return type.toString();
	}

	public List getSubActions() {
		return null;
	}

	public void clickInit(Selection selection, GestureTarget destItem) {
		isLegal = true;
		if (selection.getLinks().size() < 1) {
			isLegal = false;
			return;
		}
		for (Link tr : selection.getLinks()) {
			if (tr.getType().equals(type)) {
				isLegal = false;
				return;
			}
		}
		this.sources = selection;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {

		List<HistoryItem> historyList = new LinkedList<HistoryItem>();
		List<TreePath> pathouts = new LinkedList<TreePath>();

		for (TreePath path : sources.getPaths()) {

			Link tr = (Link) path.getLastPathComponent();

			TreePath outpath = path.getParentPath()
					.pathByAddingChild(
							new OBORestrictionImpl(tr.getChild(), tr
									.getParent(), type));

			historyList.add(new LinkTypeHistoryItem(tr, type));
			pathouts.add(outpath);
		}

		HistoryItem item;
		if (historyList.size() == 1) {
			item = historyList.get(0);
		} else {
			item = new TermMacroHistoryItem("Changed relationship types to "
					+ type);
			for (int i = 0; i < historyList.size(); i++) {
				((TermMacroHistoryItem) item).addItem(historyList.get(i));
			}
		}

		TreePath[] outpatharr = new TreePath[pathouts.size()];
		for (int i = 0; i < outpatharr.length; i++)
			outpatharr[i] = pathouts.get(i);
		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromPaths(null, outpatharr, null,
						SessionManager.getManager().getCurrentLinkDatabase(),
						RootAlgorithm.GREEDY, true));

		return item;
	}
}
