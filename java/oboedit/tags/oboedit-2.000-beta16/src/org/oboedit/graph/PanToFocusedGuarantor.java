package org.oboedit.graph;

import java.awt.geom.Rectangle2D;

import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.BoundsUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.util.PBounds;

public class PanToFocusedGuarantor extends BoundsGuarantorCycleState {

	public PanToFocusedGuarantor(LinkDatabaseCanvas canvas) {
		setDesc("Pan to focused node");
		setCanvas(canvas);
	}

	public boolean getZoom() {
		return false;
	}
	
	public PNode getFocusedNode() {
		PNode node = canvas.getFocusedNode();
		if (node == null)
			return null;
		if (canvas.isLayingOut()) {
			
			PNode layoutNode = null;
			if (node instanceof OENode) {
				layoutNode = canvas.getFinalLayoutVersion(((OENode) node).getObject());
			} else if (node instanceof OELink)
				layoutNode = canvas.getFinalLayoutVersion(((OELink) node).getObject());
			if (layoutNode != null)
				node = layoutNode;
		}
		return node;
	}
	
	public Rectangle2D getNewBounds() {
		if (getFocusedNode() == null)
			return null;
		return getFocusedNode().getFullBounds();
	}

}
