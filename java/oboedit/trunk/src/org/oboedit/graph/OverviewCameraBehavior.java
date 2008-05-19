package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.bbop.swing.ShapeUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.PLayer;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PAffineTransform;
import edu.umd.cs.piccolo.util.PPaintContext;
import edu.umd.cs.piccolox.handles.PBoundsHandle;
import edu.umd.cs.piccolox.nodes.PClip;

import org.apache.log4j.*;

public class OverviewCameraBehavior implements ViewBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OverviewCameraBehavior.class);

	protected PCamera widget;
	protected PPath viewRect = new PPath();
	protected PPath cameraWrapper = new PPath();
	protected PClip clip = new PClip();
	/*
	 * protected PropertyChangeListener widgetListener = new
	 * PropertyChangeListener() { public void propertyChange(PropertyChangeEvent
	 * evt) { adjustZoom(); } };
	 */

	protected ComponentListener componentListener = new ComponentAdapter() {
		public void componentResized(ComponentEvent e) {
			placeCamera();
		}
	};
	
	protected PropertyChangeListener cameraFrameListener = new PropertyChangeListener() {
		public void propertyChange(PropertyChangeEvent evt) {
			placeCamera();
		}
	};

	protected PropertyChangeListener cameraListener = new PropertyChangeListener() {
		public void propertyChange(PropertyChangeEvent evt) {
			resetCameraView();
		}
	};
	
	protected void resetCameraView() {
		widget.setViewBounds(canvas.getLayer().getFullBoundsReference());
		Rectangle2D r = canvas.getCamera().getViewBounds();
		r = widget.viewToLocal(r);
		viewRect.setPathTo(r);	
	}

	protected LinkDatabaseCanvas canvas;

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		viewRect.setStroke(new BasicStroke(1));
		viewRect.setStrokePaint(Color.green);
		widget = new PCamera() {
			@Override
			protected void paint(PPaintContext paintContext) {
				int quality = paintContext.getRenderQuality();
				paintContext.setRenderQuality(PPaintContext.LOW_QUALITY_RENDERING);
				super.paint(paintContext);
				paintContext.setRenderQuality(quality);
			}
		};
		widget.addLayer(canvas.getLayer());
		clip.setPickable(false);
		clip.setChildrenPickable(false);
		cameraWrapper.setPaint(Preferences.defaultBackgroundColor());
		cameraWrapper.setStrokePaint(Preferences.defaultButtonColor());
		cameraWrapper.setPathTo(ShapeUtil.createRoundRectangle(0, 0, 100, 50));
		cameraWrapper.addInputEventListener(new PBasicInputEventHandler() {
			@Override
			public void mouseDragged(PInputEvent event) {
//				logger.info("event = "+event);
			}
		});
		clip.setPathTo(ShapeUtil.createRoundRectangle(0, 0, 100, 50));
		cameraWrapper.addPropertyChangeListener(PNode.PROPERTY_BOUNDS, cameraFrameListener);
//		widget.addChild(viewRect);
		clip.addChild(widget);
		clip.addChild(viewRect);
		cameraWrapper.addChild(clip);
		canvas.getCamera().addChild(cameraWrapper);
		PBoundsHandle.addBoundsHandlesTo(cameraWrapper);
		placeCamera();
		canvas.addComponentListener(componentListener);
		canvas.getCamera().addPropertyChangeListener(
				PCamera.PROPERTY_VIEW_TRANSFORM, cameraListener);
	}

	public void placeCamera() {
		cameraWrapper.setOffset(canvas.getWidth() - 10 - cameraWrapper.getWidth(), canvas
				.getHeight() - cameraWrapper.getHeight() - 10);
		cameraWrapper.setX(0);
		cameraWrapper.setY(0);
		widget.setWidth(cameraWrapper.getWidth());
		widget.setHeight(cameraWrapper.getHeight());
		clip.setWidth(cameraWrapper.getWidth());
		clip.setHeight(cameraWrapper.getHeight());
		clip.setStroke(null);
		clip.setPaint(null);
		clip.setOffset(0,0);
		widget.setOffset(0,0);
		widget.setX(0);
		widget.setY(0);
		clip.setX(0);
		clip.setY(0);
		resetCameraView();
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.getCamera().removePropertyChangeListener(
				PCamera.PROPERTY_VIEW_TRANSFORM, cameraListener);
		PBoundsHandle.removeBoundsHandlesFrom(cameraWrapper);
		canvas.getCamera().removeChild(cameraWrapper);
		widget = null;
	}

}
