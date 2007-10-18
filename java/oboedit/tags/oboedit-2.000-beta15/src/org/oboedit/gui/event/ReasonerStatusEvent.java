package org.oboedit.gui.event;

import java.util.*;

public class ReasonerStatusEvent extends EventObject {

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
