package org.oboedit.graph;

import java.awt.geom.Rectangle2D;

import org.oboedit.gui.components.LinkDatabaseCanvas;

public class ZoomToAllGuarantor extends BoundsGuarantorCycleState {
	
	protected double maxScale = Double.MAX_VALUE;
	
	public ZoomToAllGuarantor(LinkDatabaseCanvas canvas) {
		this(canvas, Double.MAX_VALUE);
	}
	
	public ZoomToAllGuarantor(LinkDatabaseCanvas canvas, double maxScale) {
		setDesc("Zoom to all");
		setCanvas(canvas);
		this.maxScale = maxScale;
	}
	
	public Rectangle2D getNewBounds() {
		if (canvas.isLayingOut()) {
			return canvas.getNewLayer().getFullBoundsReference();
		}
		return canvas.getLayer().getFullBoundsReference();
	}
}
