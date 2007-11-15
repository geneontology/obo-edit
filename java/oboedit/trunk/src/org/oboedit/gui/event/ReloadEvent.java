package org.oboedit.gui.event;

import java.util.EventObject;

public class ReloadEvent extends EventObject {
	
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
}
