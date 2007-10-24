package org.oboedit.graph;

import java.awt.Shape;

import edu.umd.cs.piccolo.PNode;

public interface NodeFactory {
	public PNode createNode(Object key, Shape s);
}
