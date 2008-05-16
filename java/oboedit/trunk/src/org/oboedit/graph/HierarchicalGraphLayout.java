package org.oboedit.graph;

import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.IdentifiedObject;

import org.apache.log4j.*;

public class HierarchicalGraphLayout implements GraphLayout {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HierarchicalGraphLayout.class);
	//
	// Magic constants.
	// WARNING! Playing with these constants will mess with yo6ur head.
	//

	/** Ratio of maximum edge vertical distance to horizontal distance */
	public int edgeLengthHeightRatio = 20; // originally 6

	/**
	 * Number of passes up and down the levels to attempt to optimise node
	 * positions
	 */
	public final int reorderIterations = 10;

	/** Minimum gap between levels */
	public final int minLevelGap = 20; // originally 10

	/** Levels may be split if they have more than this number of nodes */
	public final int maxLevelSize = 100;

	/** Edges running though levels will be allocated this much horizontal space */
	public int insertedEdgeWidth = 20; // originally 20

	/** Horizontal gap between nodes */
	public int withinLevelGap = 20;

	// Public interface

	/** Parent nodes at top of graph layout */
	public final static int PARENT_TOP = 0;

	/** Parent nodes at left of graph layout */
	public final static int PARENT_LEFT = 1;

	/** Parent nodes at left of graph layout */
	public final static int PARENT_RIGHT = 2;

	/** Parent nodes at left of graph layout */
	public final static int PARENT_BOTTOM = 3;

	protected Map<Object, Shape> posMap = new HashMap<Object, Shape>();

	protected Map<DummyEdge, Shape> edgeShapes = new HashMap<DummyEdge, Shape>();;

	protected Map<Object, NodeObj> nodes = new LinkedHashMap<Object, NodeObj>();

	protected Collection<DummyEdge> dummyEdges = new LinkedHashSet<DummyEdge>();

	protected boolean drawEdgesToNodeCenters = false;

	/*
	 * protected Map<NodeObj, Collection<DummyEdge>> dummyEdges = new
	 * LinkedHashMap<NodeObj, Collection<DummyEdge>>();
	 */

	protected DummyEdge scratchEdge = new DummyEdge(new NodeObj("parent_str", -1),
			new NodeObj("parent_str", -2), true);

	//

	// Internal implementation

	// fields

	private int orientation = PARENT_TOP;

	private ArrayList levels = new ArrayList();

	private Collection<DummyEdge> originalEdges;

	protected class NodeObj {
		protected Object o;

		protected int width;

		protected int height;

		protected Level level;

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

		public int getLayoutHeight() {
			if (orientation == PARENT_TOP || orientation == PARENT_BOTTOM)
				return height;
			else
				return width;
		}

		public int getHeight() {
			return height;
		}

		public void setHeight(int height) {
			this.height = height;
		}

		public Level getLevel() {
			return level;
		}

		public void setLevel(Level level) {
			this.level = level;
			if (level == null)
				(new Exception("null level specified for " + getNode()))
						.printStackTrace();
		}

		public int getWidth() {
			return width;
		}

		public int getLayoutWidth() {
			if (orientation == PARENT_TOP || orientation == PARENT_BOTTOM)
				return width;
			else
				return height;
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

	private class Level {

		int levelNumber;

		// int size;
		int location, depth;

		List<NodeObj> nodes = new ArrayList<NodeObj>();

		public Level(int levelNumber) {
			this.levelNumber = levelNumber;
		}

		public void addNode(NodeObj nodeObject) {
			nodes.add(nodeObject);
		}

		void reorder(Level above, Level below) {

			for (int j = 0; j < nodes.size(); j++) {

				NodeObj nj = (NodeObj) nodes.get(j);

				double total = 0;
				int connected = 0;

				if (above != null)
					for (int i = 0; i < above.nodes.size(); i++) {
						NodeObj ni = (NodeObj) above.nodes.get(i);

						if (isConnected(ni, nj)) {
							connected++;
							total += ni.getLocation();
						}
					}

				if (below != null)
					for (int i = 0; i < below.nodes.size(); i++) {
						NodeObj ni = (NodeObj) below.nodes.get(i);

						if (isConnected(ni, nj)) {
							connected++;
							total += ni.getLocation();
						}
					}

				if (connected == 0) {
					continue;
					// throw new RuntimeException("No connected Nodes");
				} else {
					total /= connected;
				}

				nj.setLocation((int) total);
			}

			while (true) {

				Collections.sort(nodes, nodeLayoutComparator);

				boolean foundOverlap = false;
				for (int i = 1; i < nodes.size(); i++) {
					NodeObj a = (NodeObj) nodes.get(i - 1);
					NodeObj b = (NodeObj) nodes.get(i);

					int overlap = minLevelGap
							+ (a.getLocation() + a.getLayoutWidth() / 2)
							- (b.getLocation() - b.getLayoutWidth() / 2);
					if (overlap > 0) {
						foundOverlap = true;
						a.setLocation(a.getLocation() - overlap / 2 - 1);
						b.setLocation(b.getLocation() + overlap / 2 + 1);
					}
				}
				if (!foundOverlap)
					break;
			}
		}

		void calcInitialPositions() {

			int width = 0;
			for (int i = 0; i < nodes.size(); i++) {
				NodeObj n = (NodeObj) nodes.get(i);

				n.setLocation(width + n.getLayoutWidth() / 2);
				width += n.getLayoutWidth() + withinLevelGap;
			}

		}

		void shiftLeft(int delta) {
			for (int i = 0; i < nodes.size(); i++) {
				NodeObj node = (NodeObj) nodes.get(i);
				((NodeObj) nodes.get(i))
						.setLocation(node.getLocation() - delta);
			}
		}

		void setDepth(int depth) {
			this.depth = depth;

		}

		void setLocation(int location) {
			this.location = location;
		}

		int getWidth() {
			final NodeObj nd = ((NodeObj) nodes.get(nodes.size() - 1));
			return nd.getLocation() + nd.getLayoutWidth() / 2;
		}

		int getStart() {
			final NodeObj nd = ((NodeObj) nodes.get(0));
			return nd.getLocation() - nd.getLayoutWidth() / 2;
		}

	}

	/**
	 * After calling layout() call getWidth and getHeight <br/> All nodes will
	 * be in this bounding box. <br/>
	 * 0&lt;node.x+/-node.getWidth/2&lt;layout.getWidth <br/>
	 * 0&lt;node.y+/-node.getHeight/2&lt;layout.getHeight <br/> Noting that x
	 * and y are the centres of the nodes. All edge routes will also be in the
	 * bounding box.
	 * 
	 * @return width of layout
	 */

	public int getWidth() {
		int maxWidth = 0;

		for (Iterator i = levels.iterator(); i.hasNext();)
			maxWidth = Math.max(maxWidth, ((Level) i.next()).getWidth());

		return maxWidth;

	}

	/** See getWidth() */
	public int getHeight() {
		Level l = (Level) levels.get(levels.size() - 1);
		return l.location + l.depth / 2;
	}

	private Comparator nodeLayoutComparator = new Comparator() {
		// Classcast exception if not NodeLayoutData
		public int compare(Object o1, Object o2) {
			int val = ((NodeObj) o1).getLocation()
					- ((NodeObj) o2).getLocation();
			if (val == 0 && o1 instanceof NodeObj && o2 instanceof NodeObj)
				return objectComparator.compare((NodeObj) o1, (NodeObj) o2);
			else
				return val;
		}

	};

	public boolean isConnected(NodeObj a, NodeObj b) {
		long time = System.currentTimeMillis();
		scratchEdge.setChildParent(a, b);
		if (dummyEdges.contains(scratchEdge)) {
			return true;
		}
		scratchEdge.setChildParent(b, a);
		if (dummyEdges.contains(scratchEdge)) {
			return true;
		}
		return false;
		/*
		 * boolean returnVal = false; for (DummyEdge edge : dummyEdges) { if
		 * ((edge.getChild().equals(a) && edge.getParent().equals(b)) ||
		 * (edge.getChild().equals(b) && edge.getParent().equals(b))) {
		 * returnVal = true; break; } } return returnVal;
		 */
	}

	public Collection<NodeObj> getParents(NodeObj child) {
		Collection<NodeObj> out = new LinkedList<NodeObj>();
		for (DummyEdge edge : dummyEdges) {
			if (edge.getLayoutChild().equals(child)) {
				out.add(edge.getLayoutParent());
			}
		}
		return out;
	}

	/**
	 * Compute layout. This method finally calls setLocation on all the nodes
	 * and setRoute on all the edges.
	 */

	public void reset() {
		nodes = new LinkedHashMap<Object, NodeObj>();
		dummyEdges.clear();
	}

	protected Comparator<NodeObj> objectComparator = new Comparator<NodeObj>() {
		public int compare(NodeObj o1, NodeObj o2) {
			Object oa = o1.getNode();
			Object ob = o2.getNode();
			if (oa instanceof IdentifiedObject
			    && ob instanceof IdentifiedObject) {
				IdentifiedObject a = (IdentifiedObject) o1.getNode();
				IdentifiedObject b = (IdentifiedObject) o2.getNode();
				// Dangling objects have null names (Chris did it that way on purpose)
				String aName = a.getName();
				if (aName == null)
				    return -1;
				String bName = b.getName();
				if (bName == null)
				    return 1;
				return aName.compareToIgnoreCase(bName);
			} else if (ob instanceof IdentifiedObject)
				return -1;
			else if (oa instanceof IdentifiedObject)
				return 1;
			else
				return 0;
		}
	};

	public void setObjectComparator(Comparator<NodeObj> objectComparator) {
		this.objectComparator = objectComparator;
	}
	
	protected int nodeCount = 0;

	public void doLayout() {
		long time = System.currentTimeMillis();
		levels.clear();

		ArrayList<NodeObj> nodeList = new ArrayList<NodeObj>(nodes.values());
		boolean [] seenem = new boolean[nodes.size()];
		Collections.sort(nodeList, new Comparator<NodeObj>() {

			public int compare(NodeObj o1, NodeObj o2) {
				return objectComparator.compare(o1, o2);
			}
		});

		for (Iterator i = nodeList.iterator(); i.hasNext();) {
			NodeObj n = (NodeObj) i.next();
			findLevel(maxLevelSize, n, seenem);
		}

		rationalise();

		for (Iterator i = levels.iterator(); i.hasNext();) {
			Level l = (Level) i.next();
			l.calcInitialPositions();

		}

		orderNodesInLevels();

		calcLevelLocations();

		int minStart = Integer.MAX_VALUE;

		for (Iterator i = levels.iterator(); i.hasNext();)
			minStart = Math.min(minStart, ((Level) i.next()).getStart());

		for (Iterator i = levels.iterator(); i.hasNext();)
			((Level) i.next()).shiftLeft(minStart);
		posMap = new HashMap<Object, Shape>();
		edgeShapes = new HashMap<DummyEdge, Shape>();

		int multiplier = 1;
		if (orientation == PARENT_BOTTOM || orientation == PARENT_RIGHT) {
			multiplier = -1;
		}

		for (NodeObj n : nodes.values()) {
			if (n.isDummy())
				continue;
			Point2D offset = new Point2D.Double();
			if (orientation == PARENT_TOP || orientation == PARENT_BOTTOM)
				offset.setLocation(n.getLocation(), multiplier
						* n.getLevel().location);
			else
				offset.setLocation(multiplier * n.getLevel().location, n
						.getLocation());

			offset.setLocation(offset.getX() - n.getWidth() / 2, offset.getY()
					- n.getHeight() / 2);
			Rectangle r = new Rectangle((int) offset.getX(), (int) offset
					.getY(), n.getWidth(), n.getHeight());

			posMap.put(n.getNode(), r);
		}
		for (DummyEdge n : dummyEdges) {
			if (n.isDummy())
				continue;

			GeneralPath shape = new GeneralPath();

			for (DummyEdge edge : n.getComponentEdges()) {

				NodeObj parent = edge.getLayoutParent();
				NodeObj child = edge.getLayoutChild();

				int parentLocation = parent.getLocation();
				int childLocation = child.getLocation();

				int levelParent = parent.getLevel().location
						+ parent.getLevel().depth / 2;
				int levelChild = child.getLevel().location
						- child.getLevel().depth / 2;

				int levelCentre = (levelParent + levelChild) / 2;

				int nodeParent;
				int nodeChild;
				if (drawEdgesToNodeCenters) {
					nodeParent = parent.getLevel().location;
					nodeChild = child.getLevel().location;
				} else {
					nodeParent = parent.getLevel().location
							+ parent.getLayoutHeight() / 2;
					nodeChild = child.getLevel().location
							- child.getLayoutHeight() / 2;
				}
				if (orientation == PARENT_TOP || orientation == PARENT_BOTTOM) {
					if (shape.getCurrentPoint() == null)
						shape.moveTo(parentLocation, multiplier * nodeParent);
					else
						shape.lineTo(parentLocation, multiplier * nodeParent);
					shape.lineTo(parentLocation, multiplier * levelParent);
					shape.curveTo(parentLocation, multiplier * levelCentre,
							childLocation, multiplier * levelCentre,
							childLocation, multiplier * levelChild);
					shape.lineTo(childLocation, multiplier * nodeChild);

				} else {
					if (shape.getCurrentPoint() == null)
						shape.moveTo(multiplier * nodeParent, parentLocation);
					else
						shape.lineTo(multiplier * nodeParent, parentLocation);
					shape.lineTo(multiplier * levelParent, parentLocation);
					shape.curveTo(multiplier * levelCentre, parentLocation,
							multiplier * levelCentre, childLocation, multiplier
									* levelChild, childLocation);
					shape.lineTo(multiplier * nodeChild, childLocation);
				}
			}

			if (orientation == PARENT_TOP || orientation == PARENT_LEFT)
				shape = (GeneralPath) ShapeUtil.reverseShape(shape, null);

			edgeShapes.put(n, shape);
		}
	}

	// methods

	private int findLevel(int maxLevelSize, NodeObj node, boolean [] seenem) {

		if (node.getLevel() != null)
			return node.getLevel().levelNumber;

		if (seenem[node.name])
			return 0;

		seenem[node.name] = true;
		int maxParentLevel = -1;

		Collection<NodeObj> parents = getParents(node);

		for (NodeObj parent : parents) {
			if (parent == null)
				continue;
			int l = findLevel(maxLevelSize, parent, seenem);
			if (l > maxParentLevel)
				maxParentLevel = l;
		}

		int levelNumber = maxParentLevel + 1;

		while (true) {

			while (levelNumber >= levels.size())
				levels.add(new Level(levels.size()));

			if (((Level) levels.get(levelNumber)).nodes.size() < maxLevelSize)
				break;

			levelNumber++;
		}

		node.setLevel((Level) levels.get(levelNumber));

		node.getLevel().addNode(node);

		return levelNumber;
	}

	private void rationalise(DummyEdge e) {
		int parentLevel = e.getLayoutParent().getLevel().levelNumber;
		int childLevel = e.getLayoutChild().getLevel().levelNumber;

		if (parentLevel < childLevel - 1) {
			// logger.info("Rationalise "+parentLevel+" "+childLevel);
			NodeObj a = e.getLayoutParent();
			for (int i = parentLevel + 1; i <= childLevel; i++) {

				NodeObj b;
				if (i == childLevel) {
					b = e.getLayoutChild();
				} else {
					Object o = new Object();
					b = new NodeObj(o, -1, true);
					b.setHeight(-1);
					b.setWidth(insertedEdgeWidth);
					b.setLevel((Level) levels.get(i));
					b.getLevel().addNode(b);
				}
				if (!nodes.containsKey(b.getNode()))
					nodes.put(b.getNode(), b);
				DummyEdge insertedEdge = new DummyEdge(b, a, true);
				dummyEdges.add(insertedEdge);
				e.getComponentEdges().add(insertedEdge);

				a = b;
			}

		} else {
			e.getComponentEdges().add(e);
			// dummyEdges.add(e)
		}
		dummyEdges.add(e);
	}

	private void rationalise() {
		originalEdges = new LinkedList<DummyEdge>(dummyEdges);

		dummyEdges.clear();

		for (DummyEdge e : originalEdges) {
			rationalise(e);
		}
	}

	private void orderNodesInLevels() {
		for (int j = 0; j < reorderIterations; j++) {

			int s = levels.size();

			for (int i = 0; i < s; i++) {
				Level p = (i == 0) ? null : (Level) levels.get(i - 1);
				Level l = (Level) levels.get(i);
				Level n = (i == s - 1) ? null : (Level) levels.get(i + 1);
				l.reorder(p, n);
			}

			for (int i = s - 1; i >= 0; i--) {
				Level p = (i == 0) ? null : (Level) levels.get(i - 1);
				Level l = (Level) levels.get(i);
				Level n = (i == s - 1) ? null : (Level) levels.get(i + 1);
				l.reorder(p, n);
			}

		}
	}

	private void calcLevelLocations() {
		int height = 0;

		Level p = null;

		for (Iterator i = levels.iterator(); i.hasNext();) {

			Level l = (Level) i.next();

			int maxLength = 0;

			// Calculate maximum edge length

			if (p != null) {
				for (Iterator i2 = l.nodes.iterator(); i2.hasNext();) {
					NodeObj n1 = (NodeObj) i2.next();
					for (Iterator i3 = p.nodes.iterator(); i3.hasNext();) {
						NodeObj n2 = (NodeObj) i3.next();
						if (isConnected(n1, n2)) {
							maxLength = Math.max(maxLength, Math.abs(n1
									.getLocation()
									- n2.getLocation()));
						}
					}
				}
				height += Math.max(minLevelGap, maxLength
						/ edgeLengthHeightRatio);
			}

			int maxHeight = 0;

			for (Iterator i2 = l.nodes.iterator(); i2.hasNext();) {
				maxHeight = Math.max(maxHeight, ((NodeObj) i2.next())
						.getLayoutHeight());
			}

			l.setDepth(maxHeight);

			height += l.depth / 2;

			l.setLocation(height);

			height += maxHeight;

			p = l;
		}
	}

	public void addEdge(Object child, Object parent) {
		NodeObj childNode = getNode(child);
		NodeObj parentNode = getNode(parent);
		DummyEdge edge = new DummyEdge(childNode, parentNode);
		dummyEdges.add(edge);
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
