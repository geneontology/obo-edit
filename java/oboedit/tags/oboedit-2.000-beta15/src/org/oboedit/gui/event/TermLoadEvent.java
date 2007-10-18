package org.oboedit.gui.event;

import java.util.EventObject;

import org.obo.datamodel.IdentifiedObject;

public class TermLoadEvent extends EventObject {

	protected IdentifiedObject io;
	
	public TermLoadEvent(Object source, IdentifiedObject io) {
		super(source);
		this.io = io;
	}
	
	public IdentifiedObject getObject() {
		return io;
	}
}
