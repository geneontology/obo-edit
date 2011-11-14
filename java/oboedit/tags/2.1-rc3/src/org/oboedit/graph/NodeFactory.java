package org.oboedit.graph;

import java.awt.Shape;

import org.oboedit.gui.components.LinkDatabaseCanvas;


import edu.umd.cs.piccolo.PNode;

public interface NodeFactory {
	public PNode createNode(Object key, Shape s);
	public void setCanvas(LinkDatabaseCanvas canvas);
}
