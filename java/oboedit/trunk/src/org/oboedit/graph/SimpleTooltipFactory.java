package org.oboedit.graph;

import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.nodes.PText;

public class SimpleTooltipFactory extends AbstractTooltipFactory {
	
	protected static SimpleTooltipFactory factory;
	
	public static SimpleTooltipFactory getInstance() {
		if (factory == null)
			factory = new SimpleTooltipFactory();
		return factory;
	}

	public PNode getTooltip(LinkDatabaseCanvas canvas, PNode node) {
		PText text = null;
		if (node.getAttribute("tooltipText") != null)
			text = new PText((String) node.getAttribute("tooltipText"));
		return text;
	}

}
