package org.oboedit.graph;

import java.awt.Shape;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.PNode;

public class DefaultNodeFactory implements NodeFactory {

	protected TypeIconManager iconManager;
	protected TypeColorManager colorManager;
	protected LinkDatabaseCanvas canvas;

	public DefaultNodeFactory() {
		// TODO Auto-generated constructor stub
	}

	public PNode createNode(Object lo, Shape s) {
		if (lo instanceof LinkedObject) {
			OENode node = new OENode((LinkedObject) lo, canvas, s);
			return node;
		} else if (lo instanceof Link) {
			OELink node = new OELink(canvas, (Link) lo, iconManager,
					colorManager, s);
			node.setTooltipFactory(new LinkTooltipFactory());
			return node;
		}
		// TODO Auto-generated method stub
		return null;
	}

	public TypeColorManager getColorManager() {
		return colorManager;
	}

	public void setColorManager(TypeColorManager colorManager) {
		this.colorManager = colorManager;
	}

	public TypeIconManager getIconManager() {
		return iconManager;
	}

	public void setIconManager(TypeIconManager iconManager) {
		this.iconManager = iconManager;
	}

	public LinkDatabaseCanvas getCanvas() {
		return canvas;
	}

	public void setCanvas(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
	}

}
