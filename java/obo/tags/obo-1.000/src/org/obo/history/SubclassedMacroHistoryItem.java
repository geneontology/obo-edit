package org.obo.history;

import org.obo.datamodel.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

public abstract class SubclassedMacroHistoryItem extends TermMacroHistoryItem {

	public SubclassedMacroHistoryItem(String str) {
		super(str);
	}

	@Override
	public void addItem(HistoryItem item) {
	}

	@Override
	public void removeItem(HistoryItem item) {
	}

	@Override
	public void setHistoryItems(Vector v) {
	}

	@Override
	public OperationWarning lock(OBOSession history) {
		historyItems = new ArrayList();
		OperationWarning ow = getItems(history, historyItems);
		OperationWarning ow2 = super.lock(history);
		if (ow == null) {
			return ow2;
		} else {
			if (ow2 != null)
				ow.addWarning(ow2);
			return ow;
		}
	}

	protected abstract OperationWarning getItems(OBOSession history,
			List historyItems);
}
