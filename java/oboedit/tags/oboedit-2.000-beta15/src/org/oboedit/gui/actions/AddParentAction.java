package org.oboedit.gui.actions;

import java.awt.event.*;

public class AddParentAction extends CopyAction {

	public AddParentAction() {
		copyChild = false;
		dragKeyCode = KeyEvent.VK_P;
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
}
