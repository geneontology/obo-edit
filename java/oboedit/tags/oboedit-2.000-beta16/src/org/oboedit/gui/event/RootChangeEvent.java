package org.oboedit.gui.event;

import org.obo.datamodel.*;

import java.util.*;

public class RootChangeEvent extends EventObject {

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
