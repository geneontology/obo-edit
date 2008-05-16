package org.oboedit.gui.event;

import java.util.*;

import org.apache.log4j.*;

public class ReasonerStatusEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerStatusEvent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected boolean active;

	public ReasonerStatusEvent(Object source, boolean active) {
		super(source);
		this.active = active;
	}

	public boolean isActive() {
		return active;
	}
}
