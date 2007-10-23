package org.bbop.framework.event;

import java.util.EventListener;

public interface GUIComponentListener extends EventListener {

	public void componentShown(GUIComponentEvent event);
	public void componentHidden(GUIComponentEvent event);
}
