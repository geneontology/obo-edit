package org.oboedit.graph;

import java.awt.Dimension;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.util.TermUtil;
import org.oboedit.piccolo.TransitionText;
import org.oboedit.piccolo.ViewRenderedStyleText;

import edu.umd.cs.piccolo.PNode;

public class LinkDatabaseLayoutEngine {

	protected LinkDatabase linkDatabase;

	protected GraphLayout graphLayout;

	protected List<NodeSizeProvider> sizeProviders = new LinkedList<NodeSizeProvider>();

	protected NamedChildProvider provider = DefaultNamedChildProvider
			.getInstance();

	protected NodeFactory factory;

	protected int initialNodeHeight = 40;

	public LinkDatabaseLayoutEngine() {
	}

	public void setGraphLayout(GraphLayout graphLayout) {
		this.graphLayout = graphLayout;
	}

	public GraphLayout getGraphLayout() {
		return graphLayout;
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public void addSizeProvider(NodeSizeProvider provider) {
		sizeProviders.add(0, provider);
	}

	public void removeSizeProvider(NodeSizeProvider provider) {
		sizeProviders.remove(provider);
	}

	protected void initializeData(PNode oldLayer) {
		// ViewRenderedStyleText text = null;
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (TermUtil.isProperty(io) || TermUtil.isObsolete(io)
					|| !(io instanceof LinkedObject) || io.isBuiltIn())
				continue;
			graphLayout.addNode(io);
			int width = 0;
			int height = getInitialNodeHeight();
			for (NodeSizeProvider sizeProvider : sizeProviders) {
				Dimension d = sizeProvider.getSize(io);
				if (d != null) {
					if (d.getHeight() > 0)
						height = (int) d.getHeight();
					if (d.getWidth() > 0)
						width = (int) d.getWidth();
				}
			}

			// PNode oldNode = provider.getNamedChild(io, oldLayer);

			// if (text == null)
			// text = new ViewRenderedStyleText();
			//
			// text.setWidth(width);
			// text.setText(OENode.getLabelAsHTML(io, getLabel(io)));
			// height = (int) text.getHeight() + OENode.MARGIN;
			// width = (int) text.getWidth() + OENode.MARGIN;

			/*
			 * if (oldNode instanceof OENode) { int w = ((OENode)
			 * oldNode).getPreferredWidth(); int h = ((OENode)
			 * oldNode).getPreferredHeight(); if (w > width) width = w; if (h >
			 * height) height = h; }
			 */
			graphLayout.setNodeDimensions(io, width, height);
			System.err.println(":: suggested size for " + io.getID()
					+ " = width=" + width + ", height=" + height);
		}
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (io instanceof LinkedObject && !io.isBuiltIn()) {
				for (Link link : linkDatabase.getParents((LinkedObject) io)) {
					if (TermUtil.isIntersection(link))
						continue;
					graphLayout.addEdge(link.getChild(), link.getParent());
				}
			}
		}
	}

	protected String getLabel(IdentifiedObject io) {
		return io.getName();
	}

	public PNode getNewLayer(PNode oldLayer) {
		graphLayout.reset();
		initializeData(oldLayer);
		graphLayout.doLayout();
		PNode out = new PNode();
		System.err.println("LDLE.linkDatabase.objects = "
				+ linkDatabase.getObjects());
		List<IdentifiedObject> objects = new ArrayList<IdentifiedObject>();
		objects.addAll(linkDatabase.getObjects());
		Collections.sort(objects, new Comparator<IdentifiedObject>() {
			public int compare(IdentifiedObject o1, IdentifiedObject o2) {
				return o1.getName().compareToIgnoreCase(o2.getName());
			}
		});
		for (IdentifiedObject io : objects) {
			if (TermUtil.isProperty(io) || TermUtil.isObsolete(io)
					|| !(io instanceof LinkedObject) || io.isBuiltIn())
				continue;
			LinkedObject lo = (LinkedObject) io;
			Shape s = graphLayout.getNodeShape(lo);
			PNode p = factory.createNode(lo, s);

//			if (p instanceof OENode) {
//				((OENode) p).setHeight(((OENode) p).getPreferredHeight());
//				((OENode) p).setWidth(((OENode) p).getPreferredWidth());
//			}

			// PPath p = createNodeForObject(lo, s);
			provider.setNamedChild(lo, out, p);
			for (Link link : linkDatabase.getParents(lo)) {
				if (TermUtil.isIntersection(link))
					continue;
				Shape links = graphLayout.getEdgeShape(link.getChild(), link
						.getParent());
				PNode linkp = factory.createNode(link, links);
				provider.setNamedChild(link, out, linkp);
			}
		}
		return out;
	}

	public NodeFactory getFactory() {
		return factory;
	}

	public void setFactory(NodeFactory factory) {
		this.factory = factory;
	}

	public int getInitialNodeHeight() {
		return initialNodeHeight;
	}

	public void setInitialNodeHeight(int initialNodeHeight) {
		this.initialNodeHeight = initialNodeHeight;
	}
}
