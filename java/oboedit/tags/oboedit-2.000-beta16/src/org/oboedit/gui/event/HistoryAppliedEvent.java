package org.oboedit.gui.event;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;

import java.util.*;

public class HistoryAppliedEvent extends EventObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected HistoryItem item;

	public HistoryAppliedEvent(Object source, HistoryItem item) {
		super(source);
		this.item = item;
	}

	public HistoryItem getHistoryItem() {
		return item;
	}
}
