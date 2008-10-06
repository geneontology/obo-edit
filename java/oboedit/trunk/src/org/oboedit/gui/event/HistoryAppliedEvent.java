package org.oboedit.gui.event;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;

import java.util.*;

import org.apache.log4j.*;

public class HistoryAppliedEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HistoryAppliedEvent.class);
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
		logger.debug("\n\n >> HistoryAppliedEvent.getHistoryItem");
		return item;
	}
}
