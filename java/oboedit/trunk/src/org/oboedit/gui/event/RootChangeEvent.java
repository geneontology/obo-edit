package org.oboedit.gui.event;

import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class RootChangeEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RootChangeEvent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected OBOSession rootHistory;

	public RootChangeEvent(Object source, OBOSession rootHistory) {
		super(source);
		this.rootHistory = rootHistory;
	}

	public OBOSession getRootHistory() {
		return rootHistory;
	}
}
