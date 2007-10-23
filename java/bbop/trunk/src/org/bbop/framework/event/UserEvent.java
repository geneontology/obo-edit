package org.bbop.framework.event;

import java.util.EventObject;

public class UserEvent extends EventObject {
	
	protected String type;

	public UserEvent(Object source, String type) {
		super(source);
		this.type = type;
	}
	
	public String getType() {
		return type;
	}
}
