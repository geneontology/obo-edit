package org.oboedit.gui.actions;

import java.awt.event.*;

import javax.swing.KeyStroke;

import org.apache.log4j.*;

public class AddParentAction extends CopyAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddParentAction.class);

	public AddParentAction() {
		copyChild = false;
		dragTitle = "Add parent";
	}

	@Override
	public String getName() {
		return "Add parent term";
	}

	@Override
	public String getDesc() {
		return "Add parent term";
	}
	
	@Override
	public String getID() {
		return "add_parent";
	}
	
	public KeyStroke getShortcut() {
		return KeyStroke.getKeyStroke(KeyEvent.VK_P,
				java.awt.Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());

	}
}
