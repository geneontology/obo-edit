package org.bbop.swing;

import java.awt.event.FocusListener;

/**
 * Used to listen for focus hierarchy events. The difference between this
 * listener and a regular FocusListener is that this listener considers the
 * current component "focused" if it OR ANY OF ITS DESCENDANTS are focused.
 * Focus gained and focus lost events are dispatched accordingly.
 * 
 * These listeners must listen on the global KeyFocusManager, so they must be
 * added via FocusManager.addFocusHierarchyListener() rather than the
 * usual addFocusListener call
 * 
 * @author jrichter
 * 
 */
public interface FocusHierarchyListener extends FocusListener {

}
