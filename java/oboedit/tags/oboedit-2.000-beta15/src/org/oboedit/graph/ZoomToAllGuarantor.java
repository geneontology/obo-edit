package org.oboedit.graph;

import java.awt.geom.Rectangle2D;

import org.oboedit.gui.components.LinkDatabaseCanvas;

public class ZoomToAllGuarantor extends BoundsGuarantorCycleState {
	
	public ZoomToAllGuarantor(LinkDatabaseCanvas canvas) {
		setDesc("Zoom to all");
		setCanvas(canvas);
	}
	
	public Rectangle2D getNewBounds() {
		if (canvas.isLayingOut()) {
			return canvas.getNewLayer().getFullBoundsReference();
		}
		return canvas.getLayer().getFullBoundsReference();
	}
}
