package org.oboedit.graph;

import org.oboedit.gui.components.LinkDatabaseCanvas;

public interface ViewBehavior {

	public void install(LinkDatabaseCanvas canvas);

	public void uninstall(LinkDatabaseCanvas canvas);
}
