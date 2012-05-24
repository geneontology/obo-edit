package org.oboedit.graph;

import java.awt.Shape;

public interface GraphLayout {

	public void reset();
	
	public void addNode(Object node);
	public void addEdge(Object child, Object parent);
	
	public void setNodeDimensions(Object node, int width, int height);
	
	public void doLayout();
	
	public Shape getEdgeShape(Object child, Object parent);
	public Shape getNodeShape(Object node);
	
	
}
