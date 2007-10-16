package org.bbop.framework.dock.idw;

import java.util.EventListener;

import org.bbop.framework.GUIComponent;

import net.infonode.docking.View;

public interface ViewListener extends EventListener {
	public void viewCreated(View v, GUIComponent c);
	public void viewDestroyed(View v, GUIComponent c);
}
