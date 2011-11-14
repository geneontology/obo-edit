package org.oboedit.gui.actions;

import java.awt.event.*;
import java.util.*;

import org.bbop.swing.KeyRecorder;
import java.awt.Point;
import javax.swing.*;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

import javax.swing.KeyStroke;

import org.apache.log4j.*;

public class MoveAction implements ClickMenuAction, DropMenuAction,
	InputHandlerI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MoveAction.class);

	protected Selection preSelection;

	protected GestureTarget destPath;

	protected LinkedObject target;

	protected List<EditAction> actionList = new Vector<EditAction>();

	protected OBOProperty type;

	protected boolean useOriginalType = false;

	protected boolean isLegal = false;

	protected JPopupMenu dragMenu = new JPopupMenu();

	protected SpecificMoveAction copyOriginalType = new SpecificMoveAction(
			null, true);

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Move";
	}

	public String getDesc() {
		return "Move";
	}

	protected static Comparator<EditAction> moveActionSorter = new Comparator<EditAction>() {
		public int compare(EditAction a, EditAction b) {
			SpecificMoveAction sa = (SpecificMoveAction) a;
			SpecificMoveAction sb = (SpecificMoveAction) b;
			if (sa.getUseOriginalType() && sb.getUseOriginalType())
				return 0;
			if (sa.getUseOriginalType())
				return -1;
			if (sb.getUseOriginalType())
				return 1;
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

	public List<EditAction> getSubActions() {
		actionList.clear();
		actionList.add(new SpecificMoveAction(type, true));

		Iterator<OBOProperty> it = TermUtil.getRelationshipTypes(
				SessionManager.getManager().getSession()).iterator();
		while (it.hasNext()) {
			OBOProperty type = it.next();
			actionList.add(new SpecificMoveAction(type, false));
		}

		Collections.sort(actionList, moveActionSorter);

		return actionList;
	}

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof Selection) {
			useOriginalType = keyChecker.isDown(KeyEvent.VK_SHIFT);
			dropInit((Selection) o, dest);
			/*
			 * sourcePanel.setDragTitle("Move" + (useOriginalType ? " (original
			 * type)" : "..."));
			 */
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
			useOriginalType = keyChecker.isDown(KeyEvent.VK_SHIFT);
			type = null;
			dropInit((Selection) o, dest);
			if (!isLegal())
				return false;
			if (useOriginalType) {
				copyOriginalType.clickInit((Selection) o, dest);
				HistoryItem executeItem = null;
				if (copyOriginalType.isLegal())
					executeItem = copyOriginalType.execute();
				if (executeItem != null)
					SessionManager.getManager().apply(executeItem);
				else
					return false;
			} else {
				dragMenu.removeAll();
				DefaultInputHandler.buildMenu(dragMenu, getSubActions(),
						(Selection) o, dest, true, true);
				dragMenu.show(dropPanel, (int) p.getX(), (int) p.getY());
			}
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
		target = destItem.getTerm();
		if (target == null) {
			isLegal = false;
			return;
		}
		if (target instanceof AnnotatedObject
				&& ((AnnotatedObject) target).isObsolete()) {
			isLegal = false;
			return;
		}

		for (Link tr : sourceItems.getLinks()) {

			if (TermUtil.isObsolete(tr.getChild())
					|| TermUtil.isProperty(tr.getChild()) != TermUtil
							.isProperty(target)
					|| HistoryUtil.hasChild(target, tr)
					|| (!Preferences.getPreferences().getAllowCycles() && tr
							.getChild().equals(target))
					|| (!Preferences.getPreferences().getAllowCycles() && TermUtil
							.isDescendant(tr.getChild(), target))) {
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
		return null;
	}

	public String getDragDesc() {
		return "Move" + (useOriginalType ? " (original * type)" : "...");
	}
	
	public String getID() {
		return "move";
	}
	
	public KeyStroke getShortcut() {
		return KeyStroke.getKeyStroke(KeyEvent.VK_X,
				java.awt.Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
	}
}
