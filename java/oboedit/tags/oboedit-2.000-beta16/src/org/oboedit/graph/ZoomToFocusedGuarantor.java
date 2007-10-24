package org.oboedit.graph;

import java.awt.geom.Rectangle2D;

import org.oboedit.gui.components.LinkDatabaseCanvas;

public class ZoomToFocusedGuarantor extends BoundsGuarantorCycleState {
	
	public ZoomToFocusedGuarantor(LinkDatabaseCanvas canvas) {
		setDesc("Zoom to focused node");
		setCanvas(canvas);
	}
	
	public Rectangle2D getNewBounds() {
		if (canvas.getFocusedNode() == null)
			return null;
		return canvas.getFocusedNode().getGlobalFullBounds();
	}


}
