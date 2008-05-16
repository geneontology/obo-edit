package org.oboedit.gui.event;

import java.util.EventObject;

import org.apache.log4j.*;

public class PreSelectionEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PreSelectionEvent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public PreSelectionEvent(Object source) {
		super(source);
	}
}
