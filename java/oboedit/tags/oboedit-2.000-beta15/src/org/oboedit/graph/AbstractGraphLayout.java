package org.oboedit.graph;

import java.awt.Shape;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;

import org.oboedit.graph.HierarchicalGraphLayout.NodeObj;

public abstract class AbstractGraphLayout implements GraphLayout {
	protected DummyEdge scratchEdge = new DummyEdge(new NodeObj("parent_str", -1),
			new NodeObj("parent_str", -2), true);
	
	protected Map<Object, Shape> posMap = new HashMap<Object, Shape>();
	protected Map<DummyEdge, Shape> edgeShapes = new HashMap<DummyEdge, Shape>();
	protected Map<Object, NodeObj> nodes = new LinkedHashMap<Object, NodeObj>();

	protected Collection<DummyEdge> edges = new LinkedHashSet<DummyEdge>();
	
	public void reset() {
		nodes = new LinkedHashMap<Object, NodeObj>();
		edges.clear();
	}
	
	public void addEdge(Object child, Object parent) {
		NodeObj childNode = getNode(child);
		NodeObj parentNode = getNode(parent);
		DummyEdge edge = new DummyEdge(childNode, parentNode);
		edges.add(edge);
	}

	public NodeObj getNode(Object o) {
		return nodes.get(o);
	}

	public void addNode(Object node) {
		nodes.put(node, new NodeObj(node, nodes.size()));
	}

	public Shape getEdgeShape(Object child, Object parent) {
		scratchEdge.setChildParent(new NodeObj(child, -1), new NodeObj(parent, -1));
		return edgeShapes.get(scratchEdge);
	}

	public Shape getNodeShape(Object node) {
		return posMap.get(node);
	}

	public void setNodeDimensions(Object node, int width, int height) {
		NodeObj obj = nodes.get(node);
		obj.setWidth(width);
		obj.setHeight(height);
	}
	
	protected class NodeObj {
		protected Object o;

		protected int width;

		protected int height;

		protected int location;

		protected boolean isDummy = false;
		
		protected int name;

		public NodeObj(Object o, int name) {
			this(o, name, false);
		}

		public NodeObj(Object o, int name, boolean isDummy) {
			this.o = o;
			this.name = name;
			this.isDummy = isDummy;
		}

		public boolean isDummy() {
			return isDummy;
		}

		public int getLocation() {
			return location;
		}

		public Object getNode() {
			return o;
		}

		public int getHeight() {
			return height;
		}

		public void setHeight(int height) {
			this.height = height;
		}

		public int getWidth() {
			return width;
		}

		public void setWidth(int width) {
			this.width = width;
		}

		public void setLocation(int location) {
			this.location = location;
		}

		public boolean equals(Object o) {
			if (o instanceof NodeObj) {
				return ((NodeObj) o).getNode().equals(getNode());
			} else
				return false;
		}

		public int hashCode() {
			return getNode().hashCode();
		}

		public String toString() {
			return "node:" + getNode().toString();
		}
	}
	
	public class DummyEdge {
		protected NodeObj layoutChild;

		protected NodeObj layoutParent;

		/*
		 * protected NodeObj child; protected NodeObj parent;
		 */
		protected Collection<DummyEdge> componentEdges = new LinkedList<DummyEdge>();

		protected Shape shape;

		protected boolean isDummy;

		public Collection<DummyEdge> getComponentEdges() {
			return componentEdges;
		}

		public void setShape(Shape shape) {
		}

		public Shape getShape() {
			return shape;
		}

		public DummyEdge(NodeObj child, NodeObj parent) {
			this(child, parent, false);
		}

		public DummyEdge(NodeObj child, NodeObj parent, boolean isDummy) {
			setChildParent(child, parent);
			this.isDummy = isDummy;
		}

		public boolean isDummy() {
			return isDummy;
		}

		public NodeObj getLayoutChild() {
			return layoutChild;
		}

		public NodeObj getLayoutParent() {
			return layoutParent;
		}

		public void setChildParent(NodeObj child, NodeObj parent) {
			/*
			 * this.child = child; this.parent = parent; if (orientation ==
			 * PARENT_TOP || orientation == PARENT_LEFT) { this.layoutChild =
			 * child; this.layoutParent = parent; } else { this.layoutParent =
			 * child; this.layoutChild = parent; }
			 */
			this.layoutChild = child;
			this.layoutParent = parent;
		}

		public String toString() {
			return layoutChild + "-" + layoutParent;
		}

		public int hashCode() {
			return 31 * layoutChild.hashCode() + layoutParent.hashCode();
		}

		public boolean equals(Object o) {
			if (o instanceof DummyEdge) {
				DummyEdge edge = (DummyEdge) o;
				return edge.getLayoutChild().equals(layoutChild)
						&& edge.getLayoutParent().equals(layoutParent);
			} else {
				return false;
			}
		}
	}
}
