package org.oboedit.gui;

import javax.swing.KeyStroke;

public interface ClickMenuAction extends EditAction {

	public void clickInit(Selection sourceItems, GestureTarget destItem);

	public KeyStroke getKeyStroke();
}
