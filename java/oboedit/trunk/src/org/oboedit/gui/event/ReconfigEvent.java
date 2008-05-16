package org.oboedit.gui.event;

import java.util.EventObject;

import org.apache.log4j.*;

public class ReconfigEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReconfigEvent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1466090380780271380L;

	public ReconfigEvent(Object source) {
		super(source);
	}
}
