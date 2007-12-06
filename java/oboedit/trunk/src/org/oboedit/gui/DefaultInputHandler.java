package org.oboedit.gui;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.tree.TreePath;

import org.bbop.framework.GUIManager;
import org.bbop.swing.*;

import java.util.*;
import java.util.List;
import java.util.logging.Logger;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.util.GUIUtil;

public class DefaultInputHandler implements InputHandlerI {

	protected JPopupMenu dragMenu = new JPopupMenu();

	public DefaultInputHandler() {
	}

	// Handle right click menu here
	public boolean click(JComponent panel, GestureTarget dest, MouseEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		if (GUIUtil.isPopupTrigger(e, keyChecker)) {
			dragMenu.removeAll();
			Selection selection = null;
			if (panel instanceof ObjectSelector) {
				selection = ((ObjectSelector) panel).getSelection(e);
			}
			buildMenu(dragMenu,
					EditActionManager.getManager().getEditActions(), selection,
					dest, true, false);
			dragMenu.addSeparator();
			if (panel instanceof RightClickMenuProvider) {
				((RightClickMenuProvider) panel).fillInMenu(e, dragMenu);
			}
			// add a separator, and then add panel specific configurations
			dragMenu.show(panel, e.getX(), e.getY());
			return true;
		} else
			return false;
	}

	public static JMenu getMenu(final java.util.List editActions,
			final boolean fromDrop) {
		return getMenu(editActions, SelectionManager.createEmptyTarget(), true,
				fromDrop);
	}

	public static JMenu getMenu(final java.util.List editActions,
			final GestureTarget dest, final boolean showDisabled,
			final boolean fromDrop) {
		AbstractDynamicMenuItem menu = new AbstractDynamicMenuItem(
				"Edit operations", true, true, true) {

			public List<Component> getItems() {
				List<Component> out = new ArrayList<Component>();
				Selection selected = SelectionManager.getGlobalSelection();
				boolean liveSelection = (selected == null);
				if (selected == null)
					selected = SelectionManager.getGlobalSelection();

				Iterator it = editActions.iterator();
				Set<Character> inUseSet = new HashSet<Character>();
				int enabledCount = 0;
				int[] index = { 0 };
				while (it.hasNext()) {
					ClickMenuAction editAction = (ClickMenuAction) it.next();
					JMenuItem item = getMenuItem(selected, dest, editAction,
							liveSelection, fromDrop, (fromDrop ? index : null),
							inUseSet);

					if (item.isEnabled()) {
						enabledCount++;
						out.add(item);
					} else if (showDisabled)
						out.add(item);
				}
				return out;
			}
		};

		buildMenu(menu, editActions, fromDrop);
		return menu;
	}

	public static void buildMenu(JComponent menu, java.util.List editActions,
			boolean fromDrop) {
		buildMenu(menu, editActions, SelectionManager.getGlobalSelection(),
				SelectionManager.createEmptyTarget(), true, fromDrop);
	}

	public static int buildMenu(JComponent menu, java.util.List editActions,
			Selection selected, GestureTarget dest, boolean showDisabled,
			boolean fromDrop) {

		boolean liveSelection = (selected == null);
		if (selected == null)
			selected = SelectionManager.getGlobalSelection();

		Iterator it = editActions.iterator();
		Set<Character> inUseSet = new HashSet<Character>();
		int enabledCount = 0;
		int[] index = { 0 };
		while (it.hasNext()) {
			ClickMenuAction editAction = (ClickMenuAction) it.next();
			JMenuItem item = getMenuItem(selected, dest, editAction,
					liveSelection, fromDrop, (fromDrop ? index : null),
					inUseSet);
			System.err.println("item = " + item);
			if (item.isEnabled()) {
				enabledCount++;
				menu.add(item);
			} else if (showDisabled)
				menu.add(item);
		}
		return enabledCount;
	}

	public static JMenuItem getMenuItem(final Selection selection,
			final GestureTarget dest, final EditAction editAction,
			final boolean liveSelection, final boolean fromDrop, int[] index,
			Set<Character> inUse) {

		if (editAction instanceof DropMenuAction
				&& editAction instanceof ClickMenuAction) {
			if (fromDrop)
				((DropMenuAction) editAction).dropInit(selection, dest);
			else
				((ClickMenuAction) editAction).clickInit(selection, dest);
		} else if (editAction instanceof DropMenuAction)
			((DropMenuAction) editAction).dropInit(selection, dest);
		else if (editAction instanceof ClickMenuAction)
			((ClickMenuAction) editAction).clickInit(selection, dest);

		JMenuItem item;
		boolean hasChildren = editAction.getSubActions() != null
				&& editAction.getSubActions().size() > 0;

		if (hasChildren) {
			item = new JMenu(editAction.getName());
			if (editAction.isLegal()) {
				boolean foundEnabled = false;
				Iterator it = editAction.getSubActions().iterator();
				while (it.hasNext()) {
					ClickMenuAction ea = (ClickMenuAction) it.next();
					JMenuItem subItem = getMenuItem(selection, dest, ea,
							liveSelection, fromDrop, null, inUse);
					if (subItem.isEnabled())
						foundEnabled = true;
					item.add(subItem);
				}
				item.setEnabled(foundEnabled);
			} else {
				item.setEnabled(false);
			}
		} else {
			item = new JMenuItem(editAction.getName());
			/*
			 * if (editAction.getKeyStroke() != null)
			 * item.setAccelerator(editAction.getKeyStroke());
			 */
			boolean isLegal = editAction.isLegal();
			item.setEnabled(isLegal);
			if (editAction.isLegal()) {
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						Selection selected;
						if (liveSelection)
							selected = SelectionManager.getGlobalSelection();
						else
							selected = selection;

						if (editAction instanceof DropMenuAction)
							((DropMenuAction) editAction).dropInit(selected,
									dest);
						else if (editAction instanceof ClickMenuAction)
							((ClickMenuAction) editAction).clickInit(selected,
									dest);

						if (editAction.isLegal()) {
							HistoryItem item = editAction.execute();
							if (item != null)
								SessionManager.getManager().apply(item);
						}
					}
				});
			}

		}
		KeyStroke key = editAction.getKeyStroke();
		if (index != null && index[0] >= 0 && index[0] <= 9 && item.isEnabled()) {
			if (!(item instanceof JMenu)) {
				if (key == null)
					key = KeyStroke.getKeyStroke(("" + index[0]++).charAt(0),
							Toolkit.getDefaultToolkit()
									.getMenuShortcutKeyMask());
			}
			Character c = getMnemonicKey(editAction.getName(), inUse);
			if (c != null)
				item.setMnemonic(c);
		}
		if (key != null)
			item.setAccelerator(key);
		return item;
	}

	protected static Character getMnemonicKey(String name, Set<Character> inUse) {
		for (int i = 0; i < name.length(); i++) {
			char c = name.charAt(i);
			if (Character.isLetterOrDigit(c)) {
				if (!inUse.contains(c)) {
					inUse.add(c);
					return c;
				}
			}
		}
		return null;
	}

	// 
	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof Selection) {
			return InputHandlerI.ACCEPT_DROP;
		} else
			return InputHandlerI.REJECT_DROP;
		/*
		 * if (o instanceof TreePath[] && sourcePanel != null) {
		 * sourcePanel.setDragTitle("Dragging Terms"); return
		 * InputHandlerI.ACCEPT_DROP; } else { return InputHandlerI.REJECT_DROP; }
		 */
	}

	public boolean drop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {

		Selection selection = (Selection) o;
		if (selection == null)
			return false;

		dragMenu.removeAll();
		int enabledCount = 0;
		int[] index = { 0 };
		Set<Character> inUseSet = new HashSet<Character>();
		for (final DropMenuAction editAction : EditActionManager.getManager()
				.getDropMenuActions()) {

			editAction.dropInit(selection, dest);
			if (!editAction.isLegal())
				continue;
			enabledCount++;
			/*
			 * JMenuItem item = new JMenuItem(editAction.getName());
			 * item.setToolTipText(editAction.getDesc());
			 * item.setFont(Preferences.getPreferences().getFont());
			 * item.addActionListener(new ActionListener() { public void
			 * actionPerformed(ActionEvent e) {
			 * controller.apply(editAction.execute()); } }); dragMenu.add(item);
			 */
			JMenuItem item = getMenuItem(selection, dest, editAction, false,
					true, index, inUseSet);
			dragMenu.add(item);

		}
		if (enabledCount > 0) {
			Logger.getLogger("").throwing(getClass().getName(), "drop", new Exception("logging popup event"));
			dragMenu.show(dropPanel, (int) p.getX(), (int) p.getY());
			return true;
		} /*
			 * else if (enabledCount == 1) {
			 * System.err.println("DefaultInputHandler: applying "+liveAction);
			 * controller.apply(liveAction.execute()); return true; }
			 */
		else
			return false;
	}

	protected static Selection getSelectionFromObject(JComponent comp, Object o) {
		if (o instanceof TreePath[]) {
			(new Exception(
					"Someone dropped a TreePath[], but should have dropped a selection"))
					.printStackTrace();
			return SelectionManager.createSelectionFromPaths(comp,
					((TreePath[]) o), null, SessionManager.getManager()
							.getCurrentLinkDatabase(), RootAlgorithm.GREEDY,
					true);
		} else
			return null;
	}

	// do nothing
	public boolean press(JComponent panel, KeyEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		/*
		 * Iterator it = controller.getEditActions().iterator();
		 * while(it.hasNext()) { final TreePath [] selected =
		 * controller.getSelectedPaths(); EditAction editAction = (EditAction)
		 * it.next(); if (editAction.getKeyStroke() != null &&
		 * editAction.getKeyStroke().getKeyCode() == e.getKeyCode() &&
		 * (editAction.getKeyStroke().getModifiers() & e.getModifiers()) ==
		 * editAction.getKeyStroke().getModifiers()) { editAction.init(selected,
		 * null); controller.apply(editAction.execute()); return true; } }
		 */
		return false;
	}

	public String getName() {
		return "All edits";
	}

	public String getDragDesc() {
		return "Dragging...";
	}

	public String getID() {
		return "default";
	}

	public KeyStroke getShortcut() {
		return KeyStroke.getKeyStroke(KeyEvent.VK_D, java.awt.Toolkit
				.getDefaultToolkit().getMenuShortcutKeyMask());

	}
}
