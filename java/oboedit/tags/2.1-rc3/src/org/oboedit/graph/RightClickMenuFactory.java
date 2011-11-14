package org.oboedit.graph;

import java.util.List;

import javax.swing.JMenuItem;

import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.event.PInputEvent;

public interface RightClickMenuFactory {
	public static JMenuItem SEPARATOR_ITEM = new JMenuItem();
	
	public List<JMenuItem> getMenuItems(LinkDatabaseCanvas canvas, PInputEvent e);
}
