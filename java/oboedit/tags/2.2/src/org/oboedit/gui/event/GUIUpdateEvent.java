package org.oboedit.gui.event;

import java.util.EventObject;

import org.apache.log4j.*;

public class GUIUpdateEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GUIUpdateEvent.class);

	public GUIUpdateEvent(Object source) {
		super(source);
	}

}
