package org.oboedit.gui.actions;

import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.tree.TreePath;
import javax.swing.*;

import org.apache.log4j.*;

public class SpecificCopyAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SpecificCopyAction.class);

	protected OBOProperty type;

	protected boolean copyChild;

	protected Selection sources;

	protected GestureTarget dest;

	protected boolean isLegal = false;

	public SpecificCopyAction(OBOProperty type, boolean copyChild) {
		this.type = type;
		this.copyChild = copyChild;
	}

	public void setCopyChild(boolean copyChild) {
		this.copyChild = copyChild;
	}

	public KeyStroke getKeyStroke() {
		if (type.equals(OBOProperty.IS_A))
			return KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit
					.getDefaultToolkit().getMenuShortcutKeyMask());
		else
			return null;
	}

	public OBOProperty getType() {
		return type;
	}

	public String getName() {
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
		if (copyChild)
			return new OBORestrictionImpl(copied, target, type);
		else
			return new OBORestrictionImpl(target, copied, type);
	}

	public void clickInit(Selection selection, GestureTarget destItem) {
		LinkedObject target = null;
		if (destItem == null) {
			isLegal = false;
			return;
		} else {
			if (destItem.getTerm() == null) {
				isLegal = false;
				return;
			}
			dest = destItem;
			target = destItem.getTerm();
		}

		if (type == null) {
			isLegal = false;
			return;
		}

		this.sources = selection;
		for (LinkedObject tr : selection.getTerms()) {

			Link newtr = getTR(target, tr, type);
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

		List<HistoryItem> historyList = new Vector<HistoryItem>();
		List<TreePath> pathouts = new Vector<TreePath>();

		LinkedObject target = dest.getTerm();

		for (LinkedObject tr : sources.getTerms()) {

			Link newtr = getTR(target, tr, type);

			if (copyChild) {
				historyList.add(new CreateLinkHistoryItem(tr, type, target));
				TreePath outpath = dest.getPath().pathByAddingChild(newtr);

				pathouts.add(outpath);
			} else
				historyList.add(new CreateLinkHistoryItem(target, type, tr));
		}
		if (!copyChild)
			pathouts.add(dest.getPath());

		HistoryItem item;
		if (historyList.size() == 1) {
			item = historyList.get(0);
		} else {
			item = new TermMacroHistoryItem("Copied terms");
			for (int i = 0; i < historyList.size(); i++) {
				((TermMacroHistoryItem) item).addItem(historyList.get(i));
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
