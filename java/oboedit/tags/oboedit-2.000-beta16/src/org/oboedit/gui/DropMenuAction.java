package org.oboedit.gui;


public interface DropMenuAction extends EditAction {
	public void dropInit(Selection sourceItems, GestureTarget destItem);
}
