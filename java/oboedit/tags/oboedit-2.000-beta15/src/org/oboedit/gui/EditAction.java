package org.oboedit.gui;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;

import java.util.List;

public interface EditAction {

	public List<EditAction> getSubActions();

	public String getName();

	public String getDesc();

	public boolean isLegal();

	public HistoryItem execute();
}
