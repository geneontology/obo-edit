package org.oboedit.gui.menu;

import java.awt.Component;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.swing.DynamicMenuItem;
import org.bbop.util.CollectionUtil;
import org.obo.datamodel.Namespace;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.DefaultInputHandler;

import org.apache.log4j.*;

public class EditMenu extends DynamicMenu {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EditMenu.class);

	public EditMenu() {
		super("Edit");
		add(createUndoSubmenu());
		add(DefaultInputHandler.getMenu(EditActionManager.getManager()
				.getEditActions(), false));
		add(createNamespaceSubmenu());

	}

	protected JMenuItem createNamespaceSubmenu() {
		AbstractDynamicMenuItem menu = new AbstractDynamicMenuItem(
				"Set default namespace", false, true, true) {
			public List<? extends Component> getItems() {
				List<Component> out = new ArrayList<Component>();
				Iterator it = SessionManager.getManager().getSession()
						.getNamespaces().iterator();
				while (it.hasNext()) {
					final Namespace ns = (Namespace) it.next();
					boolean setBold = SessionManager.getManager().getSession()
							.getDefaultNamespace().equals(ns);

					JMenuItem item = new JMenuItem(ns.toString());
					if (setBold)
						item.setFont(item.getFont().deriveFont(Font.BOLD));
					item.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							SessionManager.getManager().getSession()
									.setDefaultNamespace(ns);
						}
					});
					out.add(item);
				}
				return out;
			}
		};
		return menu;
	}

	protected JMenuItem createUndoSubmenu() {
		AbstractDynamicMenuItem menu = new AbstractDynamicMenuItem(
				"Undo submenu", true, true, true) {
			public List<? extends Component> getItems() {
				JMenuItem undoItem = new JMenuItem("Undo");
				undoItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,
						Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
				JMenuItem redoItem = new JMenuItem("Redo");
				redoItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,
						Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
								| KeyEvent.SHIFT_DOWN_MASK));
				undoItem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						SessionManager.getManager().undo();

					}
				});
				redoItem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						SessionManager.getManager().redo();
					}
				});
				undoItem.setEnabled(SessionManager.getManager().canUndo());
				redoItem.setEnabled(SessionManager.getManager().canRedo());
				return CollectionUtil.list(undoItem, redoItem);
			}
		};
		return menu;
	}
}
