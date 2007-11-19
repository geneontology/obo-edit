package org.oboedit.gui.event;

import java.util.Collection;
import java.util.Collections;
import java.util.EventObject;

import org.obo.datamodel.IdentifiedObject;

public class ExpansionEvent extends EventObject {
	
	protected Collection<IdentifiedObject> shown;
	protected Collection<IdentifiedObject> hidden;	
	
	public ExpansionEvent(Object source, Collection<IdentifiedObject> shown,
			Collection<IdentifiedObject> hidden) {
		super(source);
		if (shown == null)
			shown = Collections.emptyList();
		if (hidden == null)
			hidden = Collections.emptyList();
		this.shown = shown;
		this.hidden = hidden;
	}

	public Collection<IdentifiedObject> getShown() {
		return shown;
	}

	public Collection<IdentifiedObject> getHidden() {
		return hidden;
	}
}
