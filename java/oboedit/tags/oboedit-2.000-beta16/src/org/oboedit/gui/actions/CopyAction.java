package org.oboedit.gui.actions;

import java.awt.event.*;
import java.util.*;

import org.bbop.swing.KeyRecorder;
import java.awt.Point;
import javax.swing.*;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

import javax.swing.KeyStroke;

public class CopyAction implements ClickMenuAction, DropMenuAction,
		InputHandlerI {

	protected Selection preSelection;

	protected GestureTarget destPath;

	protected LinkedObject target;

	protected List<SpecificCopyAction> actionList = new ArrayList<SpecificCopyAction>();

	protected OBOProperty type;

	protected boolean isLegal = false;

	protected JPopupMenu dragMenu = new JPopupMenu();

	protected SessionManager sessionManager = SessionManager.getManager();

	protected SpecificCopyAction copyOriginalType = new SpecificCopyAction(
			null, true);

	protected boolean copyChild = true;

	protected String dragTitle;

	public CopyAction() {
		copyChild = true;
		dragTitle = "Add child";
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Add child term (copy)";
	}

	public String getDesc() {
		return "Add child term (copy)";
	}

	protected static Comparator<SpecificCopyAction> copyActionSorter = new Comparator<SpecificCopyAction>() {
		public int compare(SpecificCopyAction a, SpecificCopyAction b) {
			SpecificCopyAction sa = a;
			SpecificCopyAction sb = b;
			if (sa.getType().equals(OBOProperty.IS_A))
				return -1;
			if (sb.getType().equals(OBOProperty.IS_A))
				return 1;
			if (sa.getType().isBuiltIn())
				return 1;
			if (sb.getType().isBuiltIn())
				return -1;
			return sa.getName().compareToIgnoreCase(sb.getName());
		}
	};

	public List getSubActions() {
		actionList.clear();

		Iterator it = TermUtil
				.getRelationshipTypes(sessionManager.getSession()).iterator();
		while (it.hasNext()) {
			OBOProperty type = (OBOProperty) it.next();
			actionList.add(new SpecificCopyAction(type, copyChild));
		}
		Collections.sort(actionList, copyActionSorter);
		return actionList;
	}

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if ((o instanceof Selection)) {
			dropInit((Selection) o, dest);
			if (isLegal()) {
				return InputHandlerI.ACCEPT_DROP;
			} else
				return InputHandlerI.ALMOST_ACCEPT_DROP;
		}
		return InputHandlerI.REJECT_DROP;
	}

	public boolean drop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof Selection) {
			type = null;
			dropInit((Selection) o, dest);
			if (!isLegal())
				return false;

			OBOSession history = SessionManager.getManager().getSession();
			dragMenu.removeAll();
			DefaultInputHandler.buildMenu(dragMenu, getSubActions(),
					(Selection) o, dest, true, true);
			dragMenu.show(dropPanel, (int) p.getX(), (int) p.getY());
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

	public void init(Selection sourceItems, GestureTarget destItem) {
		this.preSelection = sourceItems;
		this.destPath = destItem;
		if (destItem == null) {
			isLegal = false;
			return;
		}
		isLegal = false;
		target = destItem.getTerm();
		if (target == null || TermUtil.isObsolete(target)) {
			return;
		}
		if (sourceItems.getTerms().size() < 1) {
			return;
		}
		for (LinkedObject tr : sourceItems.getTerms()) {
			if (TermUtil.isObsolete(tr)
					|| (TermUtil.isProperty(tr) != TermUtil.isProperty(target))
					|| (!Preferences.getPreferences().getAllowCycles() && tr
							.equals(target))
					|| (!Preferences.getPreferences().getAllowCycles() && TermUtil
							.isDescendant(tr, target))) {
				return;
			}
		}
		isLegal = true;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		return null;
	}

	public String getDragDesc() {
		return dragTitle + "...";
	}

	public String getID() {
		return "add_child";
	}

	public KeyStroke getShortcut() {
		return KeyStroke.getKeyStroke(KeyEvent.VK_C, java.awt.Toolkit
				.getDefaultToolkit().getMenuShortcutKeyMask());

	}
}
