package org.oboedit.graph;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.oboedit.gui.components.LinkDatabaseCanvas;

import org.apache.log4j.*;

public class ZoomWidgetBehavior implements ViewBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ZoomWidgetBehavior.class);

	protected ZoomWidget widget;
	
	protected ZoomWidgetListener widgetListener = new ZoomWidgetListener() {

		public void zoom(float zoomVal) {
			adjustZoom();
		}		
	};
	
	protected LinkDatabaseCanvas canvas;
	
	protected void adjustZoom() {
		float minZoom = canvas.getMinZoom();
		float maxZoom = canvas.getMaxZoom();
		float actualZoom = (float) (maxZoom*Math.pow(widget.getZoomValue(),2)+minZoom);
		canvas.getCamera().scaleViewAboutPoint(actualZoom / canvas.getCamera().getViewScale(), canvas.getCamera().getViewBounds().getCenterX(), canvas.getCamera().getViewBounds().getCenterY());		
	}
	
	protected int defaultWidth;
	protected int defaultHeight;
	
	public ZoomWidgetBehavior() {
		this(10, 60);
	}
	
	public ZoomWidgetBehavior(int defaultHeight, int defaultWidth) {
		this.defaultWidth = defaultWidth;
		this.defaultHeight = defaultHeight;
	}
	
	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		widget = new ZoomWidget(canvas, 10, 60);
		canvas.getCamera().addChild(widget);
		// widget.addPropertyChangeListener("ZoomWidgetValue", widgetListener);
		widget.addZoomListener(widgetListener);
		widget.setOffset(10, 10);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.getCamera().removeChild(widget);
		widget = null;
	}

}
