package org.oboedit.gui.event;

import java.util.EventObject;

import org.obo.datamodel.IdentifiedObject;

import org.apache.log4j.*;

public class TermLoadEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermLoadEvent.class);

	protected IdentifiedObject io;
	
	public TermLoadEvent(Object source, IdentifiedObject io) {
		super(source);
		this.io = io;
	}
	
	public IdentifiedObject getObject() {
		return io;
	}
}
