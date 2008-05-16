package org.oboedit.gui.event;

import java.util.EventObject;

import org.apache.log4j.*;

public class ReloadEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReloadEvent.class);
	
	protected EventObject parentEvent;
	protected boolean isHistory;
	protected boolean isFilter;
	protected boolean isReasoner;
	protected boolean isRoot;
	protected boolean isOntologyReload;
	
	public ReloadEvent(Object source, EventObject parentEvent,
			boolean isHistory,
			boolean isFilter,
			boolean isReasoner,
			boolean isRoot,
			boolean isOntologyReload) {
		super(source);
		this.parentEvent = parentEvent;
		this.isHistory = isHistory;
		this.isFilter = isFilter;
		this.isReasoner = isReasoner;
		this.isRoot = isRoot;
		this.isOntologyReload = isOntologyReload;
	}
	
	public EventObject getParentEvent() {
		return parentEvent;
	}
	
	public boolean isHistory() {
		return isHistory;
	}
	
	public boolean isReasoner() {
		return isReasoner;
	}
	
	public boolean isFilter() {
		return isFilter;
	}
	
	public boolean isRoot() {
		return isRoot;
	}
	
	public boolean isOntologyReload() {
		return isOntologyReload;
	}

	public String toString() {
		return "ReloadEvent: isHistory = " + isHistory + ", isFilter = " + isFilter + 
			", isReasoner = " + isReasoner + ", isRoot = " + isRoot +
			", isOntologyReload = " + isOntologyReload;
	}
}
