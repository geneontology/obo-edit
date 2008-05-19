package org.bbop.framework.event;

import java.util.EventObject;

import org.apache.log4j.*;

public class UserEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UserEvent.class);
	
	protected String type;

	public UserEvent(Object source, String type) {
		super(source);
		this.type = type;
	}
	
	public String getType() {
		return type;
	}
}
