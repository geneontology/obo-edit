package org.oboedit.gui.actions;

import org.bbop.swing.KeyRecorder;

import java.awt.Point;
import java.awt.event.*;
import java.util.*;
import javax.swing.tree.TreePath;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import javax.swing.*;

import org.apache.log4j.*;

public class TypeChangeAction implements ClickMenuAction, DropMenuAction,
	InputHandlerI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TypeChangeAction.class);

	protected GestureTarget dest;

	protected boolean fromDrop = false;

	protected boolean isLegal = false;

	protected OBOProperty dropType;

	protected List actionList = new Vector();

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof Selection) {

			Selection paths = (Selection) o;

			if (paths.getTerms().size() != 1
					|| !TermUtil.isProperty(paths.getTermSubSelection())
					|| TermUtil.isObsolete(paths.getTermSubSelection()))
				return InputHandlerI.REJECT_DROP;

			if (dest == null)
				return InputHandlerI.ALMOST_ACCEPT_DROP;

			if (dest.getLink().getType() == null)
				return InputHandlerI.ALMOST_ACCEPT_DROP;

			if (!dest.getLink().getType().equals(paths.getTermSubSelection()))
				return InputHandlerI.ACCEPT_DROP;
			else
				return InputHandlerI.ALMOST_ACCEPT_DROP;
		}
		return REJECT_DROP;
	}

	public boolean drop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		Selection sources = (Selection) o;
		Link tr = dest.getLink();
		OBOProperty type = (OBOProperty) sources.getTermSubSelection();
		TreePath outpath = dest.getPath().getParentPath().pathByAddingChild(
				new OBORestrictionImpl(tr.getChild(), tr.getParent(), type));
		TreePath[] post = { outpath };
		HistoryItem item = new LinkTypeHistoryItem(tr, type);
		GUIUtil.setSelections(item, sources, SelectionManager
				.createSelectionFromPaths(null, post, null, SessionManager
						.getManager().getCurrentLinkDatabase(),
						RootAlgorithm.GREEDY, true));

		SessionManager.getManager().apply(item);

		return true;
	}

	public boolean click(JComponent panel, GestureTarget dest, MouseEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		return false;
	}

	public boolean press(JComponent panel, KeyEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		return false;
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Change relationship type";
	}

	public boolean isDropAllowed() {
		return true;
	}

	public boolean isClickAllowed() {
		return true;
	}

	public String getDesc() {
		return "Change relationship type";
	}

	protected Comparator typeChangeActionSorter = new Comparator() {
		public int compare(Object a, Object b) {
			SpecificTypeChangeAction sa = (SpecificTypeChangeAction) a;
			SpecificTypeChangeAction sb = (SpecificTypeChangeAction) b;
			return sa.getType().getID().compareToIgnoreCase(
					sb.getType().getID());
		}
	};

	public List getSubActions() {
		if (fromDrop) {
			return null;
		} else {
			actionList.clear();
			Iterator it = TermUtil.getRelationshipTypes(
					SessionManager.getManager().getSession()).iterator();
			while (it.hasNext()) {
				OBOProperty type = (OBOProperty) it.next();
				actionList.add(new SpecificTypeChangeAction(type));
			}
			Collections.sort(actionList, typeChangeActionSorter);

			return actionList;
		}
	}

	public void clickInit(Selection paths, GestureTarget destItem) {
		fromDrop = false;
		this.dest = destItem;

		if (paths.getLinks().size() != 1) {
			isLegal = false;
			return;
		}
		for (Link tr : paths.getLinks()) {
			if (PathUtil.isFake(tr)) {
				isLegal = false;
				return;
			}
		}
		isLegal = true;
	}

	public void dropInit(Selection paths, GestureTarget destItem) {
		fromDrop = true;
		this.dest = destItem;

		isLegal = false;
		if (paths.getLinks().size() != 1) {
			return;
		}

		Link tr = destItem.getLink();
		if (tr == null || tr.getType() == null) {
			return;
		}

		Link dropTR = paths.getLinkSubSelection();
		if (!TermUtil.isProperty(dropTR.getChild())) {
			return;
		}

		dropType = (OBOProperty) dropTR.getChild();
		if (tr.getType().equals(dropType)) {
			return;
		}
		isLegal = true;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		if (fromDrop) {
			Link tr = dest.getLink();
			TreePath outpath = dest.getPath().getParentPath()
					.pathByAddingChild(
							new OBORestrictionImpl(tr.getChild(), tr
									.getParent(), dropType));
			HistoryItem item = new LinkTypeHistoryItem(tr, dropType);
			TreePath[] post = { outpath };

			GUIUtil.setSelections(item, SelectionManager
					.createSelectionFromTarget(dest), SelectionManager
					.createSelectionFromPaths(null, post, null, SessionManager
							.getManager().getCurrentLinkDatabase(),
							RootAlgorithm.GREEDY, true));
			return item;
		} else
			return null;
	}

	public String getDragDesc() {
		return "Setting type...";
	}
	
	public String getID() {
		return "type_change";
	}
	
       // 1/3/2008:  Midori requested that this keyboard shortcut be removed.
       // It wasn't working right, and anyway it was confusing.
       // https://sourceforge.net/tracker/index.php?func=detail&aid=1863244&group_id=36855&atid=418260
	public KeyStroke getShortcut() {
	    return null;
//		return KeyStroke.getKeyStroke(KeyEvent.VK_T,
//				java.awt.Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
	}
}
