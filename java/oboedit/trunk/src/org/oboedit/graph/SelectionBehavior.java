package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.obo.datamodel.PathCapable;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;
import edu.umd.cs.piccolo.util.PPickPath;

public class SelectionBehavior implements ViewBehavior, NodeDecorator {

	public static final Object STROKE_PATH_KEY = new Object();

	protected LinkDatabaseCanvas canvas;
	protected boolean generateSelections = false;

	protected Paint selectionPaint = Preferences.defaultSelectionColor();

	protected SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			canvas.decorate();
		}
	};
	protected PInputEventListener inputListener;
	
	public SelectionBehavior(boolean generateSelections) {
		this.generateSelections = generateSelections;
	}

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;

		if (generateSelections) {
			inputListener = new SelectionMouseListener(canvas);
			canvas.addInputEventListener(inputListener);
		}
			
		canvas.addSelectionListener(selectionListener);
		canvas.addDecorator(this);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		this.canvas = null;
		if (generateSelections) {
			canvas.removeInputEventListener(inputListener);
		}

		canvas.removeSelectionListener(selectionListener);
		canvas.removeDecorator(this);
	}

	protected static PathCapable getPathCapable(PNode node) {
		PathCapable pathCapable = null;
		if (node instanceof OENode) {
			pathCapable = ((OENode) node).getObject();
		} else if (node instanceof OELink) {
			pathCapable = ((OELink) node).getObject();
		}
		return pathCapable;
	}

	public PActivity decorate(PNode n, boolean noAnimation) {
		if (n instanceof PCNode) {
			PCNode node = (PCNode) n;
			PathCapable pathCapable = getPathCapable(node);
			PPath pathNode = node.getPathDelegate();
			if (canvas.getSelection().getAllSelectedObjects().contains(
					pathCapable)) {
				PPath strokePath = (PPath) canvas.getNamedChildProvider()
						.getNamedChild(STROKE_PATH_KEY, node);
				if (strokePath == null) {
					strokePath = new PPath();
					strokePath.setStroke(null);
					strokePath.setPaint(getSelectionPaint());
					canvas.getNamedChildProvider().setNamedChild(
							STROKE_PATH_KEY, node, strokePath);
					Shape s = (new BasicStroke(6)).createStrokedShape(pathNode
							.getPathReference());
					strokePath.setPathTo(s);
				}
				strokePath.moveInFrontOf(pathNode);
				strokePath.setVisible(true);
			} else {
				PPath strokePath = (PPath) canvas.getNamedChildProvider()
						.getNamedChild(STROKE_PATH_KEY, node);
				if (strokePath != null)
					strokePath.setVisible(false);
			}
		}
		return null;
	}

	public Paint getSelectionPaint() {
		return selectionPaint;
	}

	public void setSelectionPaint(Paint selectionPaint) {
		this.selectionPaint = selectionPaint;
	}

	public boolean onlyDecorateAfterLayout() {
		return false;
	}
}
