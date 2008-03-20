package org.oboedit.gui.actions;

import org.bbop.swing.KeyRecorder;

import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.*;
import java.util.*;
import javax.swing.tree.TreePath;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.JComponent;
import javax.swing.KeyStroke;

public class MergeAction implements ClickMenuAction, DropMenuAction,
		InputHandlerI {

	protected Selection preSelection;

	protected GestureTarget postSelection;

	protected OBOClass master;

	protected OBOClass slave;

	protected boolean isLegal = false;

    protected KeyStroke keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_M,
							   Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
							   | KeyEvent.SHIFT_MASK);
//	KeyEvent.VK_7, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()); // DEL

	public boolean isDropAllowed() {
		return true;
	}

	public boolean isClickAllowed() {
		return true;
	}

	public KeyStroke getKeyStroke() {
		return keyStroke;
	}

	public String getName() {
		return "Merge";
	}

	public String getDesc() {
		return "Merging";
	}

	public List getSubActions() {
		return null;
	}

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof Selection) {
			init((Selection) o, dest);
			// sourcePanel.setDragTitle("Merge");
			if (isLegal()) {
				return InputHandlerI.ACCEPT_DROP;
			} else
				return InputHandlerI.ALMOST_ACCEPT_DROP;
		}
		return InputHandlerI.REJECT_DROP;
	}

	public boolean drop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof Selection && keyChecker.isDown(KeyEvent.VK_M)) {
			init((Selection) o, dest);
			if (!isLegal())
				return false;
			SessionManager.getManager().apply(execute());
			return true;
		} else {
			return false;
		}
	}

	public boolean click(JComponent panel, GestureTarget dest, MouseEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		return false;
	}

	public boolean press(JComponent panel, KeyEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		return false;
	}

	public void clickInit(Selection sourceItems, GestureTarget destItem) {
		init(sourceItems, destItem);
	}

	public void dropInit(Selection sourceItems, GestureTarget destItem) {
		init(sourceItems, destItem);
	}

	public void init(Selection paths, GestureTarget destPath) {
		this.preSelection = paths;
		this.postSelection = destPath;
		if (paths == null) {
			isLegal = false;
			return;
		}
		if (paths.getTerms().size() != 1) {
			isLegal = false;
			return;
		}

		init((LinkedObject) destPath.getTerm(), (LinkedObject) paths
				.getTermSubSelection());
	}

	public void init(LinkedObject master, LinkedObject slave) {
		if (master == null || slave == null) {
			isLegal = false;
		} else if (TermUtil.isProperty(master) || TermUtil.isProperty(slave)) {
			isLegal = false;
		} else if (TermUtil.isObsolete(master) || TermUtil.isObsolete(slave)) {
			isLegal = false;
		} else if (master.equals(slave)) {
			isLegal = false;
		} else if (!TermUtil.isClass(master) || !TermUtil.isClass(slave)) {
			isLegal = false;
		} else {
			this.master = (OBOClass) master;
			this.slave = (OBOClass) slave;
			isLegal = true;
		}
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		HistoryItem item = new TermMergeHistoryItem(master, slave);

		GUIUtil.setSelections(item, preSelection, SelectionManager
				.createSelectionFromTarget(postSelection));
		return item;
	}

	public String getDragDesc() {
		return "Merge";
	}

	public String getID() {
		return "merge";
	}

	public KeyStroke getShortcut() {
//		return KeyStroke.getKeyStroke(KeyEvent.VK_M, java.awt.Toolkit
//					      .getDefaultToolkit().getMenuShortcutKeyMask()
//					      | KeyEvent.SHIFT_MASK);
	    return keyStroke;
//	    return null;
	}
}
