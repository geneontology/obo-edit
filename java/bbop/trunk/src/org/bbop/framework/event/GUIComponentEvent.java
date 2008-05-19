package org.bbop.framework.event;

import java.util.EventObject;

import org.bbop.framework.GUIComponent;

import org.apache.log4j.*;

public class GUIComponentEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GUIComponentEvent.class);
	
	protected GUIComponent component;
	protected boolean show;
	protected boolean hide;

	public GUIComponentEvent(Object source, GUIComponent component,
			boolean show, boolean hide) {
		super(source);
		this.component = component;
		this.show = show;
		this.hide = hide;
	}

	public GUIComponent getComponent() {
		return component;
	}

	public boolean isShow() {
		return show;
	}

	public boolean isHide() {
		return hide;
	}
}
