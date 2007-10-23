package org.oboedit.gui.actions;

import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.util.*;

import javax.swing.tree.TreePath;
import javax.swing.*;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

public class SpecificMoveAction implements ClickMenuAction {

	protected boolean useOriginalType;

	protected OBOProperty type;

	protected Selection sources;

	protected GestureTarget dest;

	protected boolean isLegal = false;

	public SpecificMoveAction(OBOProperty type, boolean useOriginalType) {
		this.type = type;
		this.useOriginalType = useOriginalType;
	}

	public KeyStroke getKeyStroke() {
		if (type == null)
			return KeyStroke.getKeyStroke(KeyEvent.VK_D, Toolkit
					.getDefaultToolkit().getMenuShortcutKeyMask());
		else if (type.equals(OBOProperty.IS_A))
			return KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit
					.getDefaultToolkit().getMenuShortcutKeyMask());
		else
			return null;
	}

	public OBOProperty getType() {
		return type;
	}

	public boolean getUseOriginalType() {
		return useOriginalType;
	}

	public String getName() {
		if (useOriginalType)
			return "Use original type";
		return type.toString();
	}

	public String getDesc() {
		return getName();
	}

	public List getSubActions() {
		return null;
	}

	protected Link getTR(LinkedObject target, LinkedObject copied,
			OBOProperty type) {
		return new OBORestrictionImpl(copied, target, type);
	}

	public void clickInit(Selection selection, GestureTarget destItem) {
		LinkedObject target = null;
		if (destItem == null) {
			isLegal = false;
			return;
		} else {
			if (destItem.getLink() == null) {
				isLegal = false;
				return;
			}
			dest = destItem;
			target = destItem.getTerm();
		}

		this.sources = selection;
		if (selection.getLinks().size() < 1) {
			isLegal = false;
			return;
		}
		for (Link tr : selection.getLinks()) {
			if (useOriginalType && tr.getType() == null) {
				isLegal = false;
				return;
			}
			if (useOriginalType)
				type = tr.getType();
			if (type == null) {
				isLegal = false;
				return;
			}
			Link newtr = getTR(target, tr.getChild(), type);
			if (!TermUtil.isLegalRelationship(newtr.getChild(),
					newtr.getType(), newtr.getParent())) {
				isLegal = false;
				return;
			}
		}
		isLegal = true;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {

		List historyList = new Vector();
		List pathouts = new Vector();

		LinkedObject target = dest.getTerm();

		for (Link tr : sources.getLinks()) {

			if (useOriginalType)
				type = tr.getType();

			Link newtr = getTR(target, tr.getChild(), type);

			historyList.add(new TermMoveHistoryItem(target, type, tr));

			TreePath outpath = dest.getPath().pathByAddingChild(newtr);

			pathouts.add(outpath);
		}

		HistoryItem item;
		if (historyList.size() == 1) {
			item = (HistoryItem) historyList.get(0);
		} else {
			item = new TermMacroHistoryItem("Moved terms");
			for (int i = 0; i < historyList.size(); i++) {
				((TermMacroHistoryItem) item).addItem((HistoryItem) historyList
						.get(i));
			}
		}

		TreePath[] outpatharr = new TreePath[pathouts.size()];
		for (int i = 0; i < outpatharr.length; i++)
			outpatharr[i] = (TreePath) pathouts.get(i);
		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromPaths(null, outpatharr, null,
						SessionManager.getManager().getCurrentLinkDatabase(),
						RootAlgorithm.GREEDY, true));

		return item;
	}
}
