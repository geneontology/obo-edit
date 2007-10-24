package org.oboedit.gui;

import org.obo.history.HistoryItem;

import java.util.List;

import javax.swing.KeyStroke;

public interface EditAction {

	public List<EditAction> getSubActions();

	public String getName();

	public String getDesc();

	public boolean isLegal();

	public HistoryItem execute();
	
	public KeyStroke getKeyStroke();
}
