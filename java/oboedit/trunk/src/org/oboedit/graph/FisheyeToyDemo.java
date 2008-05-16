package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.geom.*;
import java.awt.event.*;
import java.util.*;

import edu.umd.cs.piccolo.*;
import edu.umd.cs.piccolo.activities.*;
import edu.umd.cs.piccolo.nodes.*;
import edu.umd.cs.piccolox.PFrame;
import edu.umd.cs.piccolox.event.*;

import org.apache.log4j.*;

public class FisheyeToyDemo extends PFrame {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FisheyeToyDemo.class);

	public static final boolean DO_PUSH = true;
	public static final boolean DO_SCALE = true;

	public static final double MIN_SCALE = .1;
	public static final double MAX_SCALE = 3;
	public static final double SELECTION_SCALE = 5;
	public static final double GROWTH_MULTIPLIER = 30;
	public static final double PUSH_MULTIPLIER = 2;
	public static final long ANIMATION_DUR = 400;
	PSelectionEventHandler selectionEventHandler;

	public FisheyeToyDemo() {
		this(null);
	}

	public FisheyeToyDemo(PCanvas aCanvas) {
		super("NavigationExample", false, aCanvas);
	}

	public void getCenter(PNode node) {

	}

	public double getDistance(Point2D p1, Rectangle2D rect) {
		if (rect.contains(p1))
			return 0;

		double minDist = Line2D.ptSegDist(rect.getX(), rect.getY(),
				rect.getX(), rect.getY() + rect.getHeight(), p1.getX(), p1
						.getY());

		minDist = Math.min(minDist, Line2D.ptSegDist(rect.getX(), rect.getY(),
				rect.getX() + rect.getWidth(), rect.getY(), p1.getX(), p1
						.getY()));
		minDist = Math.min(minDist, Line2D.ptSegDist(rect.getX()
				+ rect.getWidth(), rect.getY(), rect.getX() + rect.getWidth(),
				rect.getY() + rect.getHeight(), p1.getX(), p1.getY()));
		minDist = Math.min(minDist, Line2D.ptSegDist(rect.getX(), rect.getY()
				+ rect.getHeight(), rect.getX() + rect.getWidth(), rect.getY()
				+ rect.getHeight(), p1.getX(), p1.getY()));

		return minDist;
	}

	public double getDistance(Point2D p1, Point2D p2) {
		return Math.sqrt(Math.pow(p1.getX() - p2.getX(), 2)
				+ Math.pow(p1.getY() - p2.getY(), 2));
	}

	public double getDistance(PNode node1, PNode node2) {
		Point2D p1 = node1.getFullBoundsReference().getCenter2D();
		Point2D p2 = node2.getFullBoundsReference().getCenter2D();
		return getDistance(p1, p2);
	}

	protected void prepFisheye() {
		Iterator it = getCanvas().getLayer().getChildrenIterator();
		while (it.hasNext()) {
			PNode node = (PNode) it.next();
			node.scaleAboutPoint(MIN_SCALE / node.getScale(), node
					.getBoundsReference().getCenter2D().getX(), node
					.getBoundsReference().getCenter2D().getY());
		}

	}

	protected Point2D lastFisheye;
	protected Map pointMap = new HashMap();

	protected void refisheye() {
		if (lastFisheye == null)
			prepFisheye();
		else
			fisheye(lastFisheye);
	}

	protected double getFisheyeScaleFactor(PNode node, Point2D focus) {
		Rectangle2D nbounds = node.localToParent(node.getBounds());
		double dist = getDistance(focus, nbounds);
		// return GROWTH_MULTIPLIER / Math.sqrt(dist);
		double scaleFactor = GROWTH_MULTIPLIER / Math.pow(dist, .5);
		double clippedScaleFactor = Math.min(Math.max(scaleFactor, MIN_SCALE),
				MAX_SCALE);
		return clippedScaleFactor;
	}

	protected double getPushAngle(Point2D mapPoint, Point2D focus) {
		return Math.atan2(mapPoint.getY() - focus.getY(), mapPoint.getX()
				- focus.getX());
	}

	protected double getPushDistance(Point2D mapPoint, Point2D focus,
			double scaleFactor) {
		double dist = Math.sqrt(Math.pow(focus.getX() - mapPoint.getX(), 2)
				+ Math.pow(focus.getY() - mapPoint.getY(), 2));
		// double newDist = Math.min(MAX_DISTANCE,
		// PUSH_MULTIPLIER/Math.sqrt(dist));
		// return newDist;
		return dist * scaleFactor * PUSH_MULTIPLIER;
	}

	protected void fisheye(Point2D focus) {
		lastFisheye = focus;
		Iterator it = getCanvas().getLayer().getChildrenIterator();
		double largestScale = 0;
		PNode largestNode = null;
		while (it.hasNext()) {
			PNode node = (PNode) it.next();

			double scaleFactor = getFisheyeScaleFactor(node, focus);
			if (scaleFactor > largestScale) {
				largestScale = scaleFactor;
				largestNode = node;
			}

			if (DO_PUSH) {
				double width = node.getBoundsReference().width;
				double height = node.getBoundsReference().height;
				double scaledWidth = node.getScale() * width;
				double scaledHeight = node.getScale() * height;
				Point2D mapPoint = (Point2D) pointMap.get(node);
				Point2D centerPoint = new Point2D.Double();
				centerPoint.setLocation(mapPoint.getX() + width / 2, mapPoint
						.getY()
						+ height / 2);

				/*
				 * Point2D mapPoint = node.getFullBounds().getOrigin(); Point2D
				 * centerPoint = node.getFullBounds().getCenter2D();
				 */

				/*
				 * Rectangle2D currentBounds =
				 * node.localToGlobal(node.getBoundsReference());
				 * centerPoint.setLocation(currentBounds.getCenterX(),
				 * currentBounds.getCenterY());
				 */
				double theta = getPushAngle(centerPoint, focus);
				double newDist = getPushDistance(centerPoint, focus,
						scaleFactor);
				double xoffset = newDist * Math.cos(theta);
				double yoffset = newDist * Math.sin(theta);
				node.setOffset(xoffset + centerPoint.getX() - scaledWidth / 2,
						yoffset + centerPoint.getY() - scaledHeight / 2);
			}
			if (DO_SCALE) {
				if (animatingNodes.contains(node)
						|| selectionEventHandler.getSelection().contains(node)) {
					scaleFactor = SELECTION_SCALE;
				}
				Point2D center = node.getBoundsReference().getCenter2D();
				node.scaleAboutPoint(scaleFactor / node.getScale(), center
						.getX(), center.getY());
			}
		}
		largestNode.moveToFront();
	}

	public void initialize() {
		final Collection nodes = new LinkedList();
		getCanvas().setPanEventHandler(null);
		getCanvas().addMouseMotionListener(new MouseMotionAdapter() {
			public void mouseMoved(MouseEvent e) {
				Point2D layerP = getCanvas().getCamera().localToView(
						new Point2D.Double(e.getX(), e.getY()));
				fisheye(layerP);
				/*
				 * PPickPath path = e.getPath(); Point2D p =
				 * path.canvasToLocal(e.getCanvasPosition(),
				 * getCanvas().getLayer());
				 */
			}
		});

		PLayer layer = getCanvas().getLayer();

		Random random = new Random();
		int ROWS = 30;
		int COLS = 30;
		for (int i = 0; i < ROWS; i++) {
			for (int j = 0; j < COLS; j++) {
				PPath each = PPath.createRectangle(0, 0, 500, 400);
				// double x = i * 10000/COLS;
				// double y = j * 10000/ROWS;
				double x = random.nextDouble() * 10000;
				double y = random.nextDouble() * 10000;
				Point2D point = new Point2D.Double(x
						- each.getBoundsReference().width / 2, y
						- each.getBoundsReference().height / 2);
				pointMap.put(each, point);
				each.offset(x, y);
				each.setPaint(new Color(random.nextFloat(), random.nextFloat(),
						random.nextFloat()));
				each.setStroke(new BasicStroke(1 + (10 * random.nextFloat())));
				each.setStrokePaint(new Color(random.nextFloat(), random
						.nextFloat(), random.nextFloat()));
				layer.addChild(each);
				nodes.add(each);
			}
		}
		refisheye();
		selectionEventHandler = new PSelectionEventHandler(getCanvas()
				.getLayer(), getCanvas().getLayer());
		getCanvas().getCamera().setViewBounds(
				new Rectangle2D.Double(0, 0, 10000, 10000));
		getCanvas().addInputEventListener(selectionEventHandler);
		PNotificationCenter.defaultCenter().addListener(this,
				"selectionChanged",
				PSelectionEventHandler.SELECTION_CHANGED_NOTIFICATION,
				selectionEventHandler);
		// getCanvas().removeInputEventListener(getCanvas().getPanEventHandler());
		// getCanvas().addInputEventListener(new PNavigationEventHandler());
	}

	protected Collection animatingNodes = new HashSet();
	protected Collection formerCollection = Collections.EMPTY_SET;

	public void selectionChanged(PNotification notfication) {
		Collection selection = selectionEventHandler.getSelection();

		Collection newNodes = new HashSet(selection);
		newNodes.removeAll(formerCollection);
		Collection missingNodes = new HashSet(formerCollection);
		missingNodes.removeAll(selection);

		Iterator it = newNodes.iterator();
		while (it.hasNext()) {
			final PNode node = (PNode) it.next();
			node.moveToFront();
			node.animateToTransparency(.7f, ANIMATION_DUR);

			Rectangle2D nbounds = node.localToParent(node.getBounds());
			final PActivity a = node
					.animateToPositionScaleRotation(
							nbounds.getX()
									- (node.getBoundsReference().getCenter2D()
											.getX() * (SELECTION_SCALE - node
											.getScale())), nbounds.getY()
									- (node.getBoundsReference().getCenter2D()
											.getY() * (SELECTION_SCALE - node
											.getScale())), SELECTION_SCALE, 0d,
							ANIMATION_DUR);

			a.setDelegate(new PActivity.PActivityDelegate() {
				public void activityStarted(PActivity activity) {
					animatingNodes.add(node);
				}

				public void activityStepped(PActivity activity) {
				}

				public void activityFinished(PActivity activity) {
					animatingNodes.remove(node);
				}
			});

		}
		it = missingNodes.iterator();
		while (it.hasNext()) {
			final PNode node = (PNode) it.next();
			Rectangle2D nbounds = node.localToParent(node.getBounds());
			final PActivity ta = node.animateToTransparency(1, ANIMATION_DUR);
			final PActivity a = node
					.animateToPositionScaleRotation(
							nbounds.getX()
									- (node.getBoundsReference().getCenter2D()
											.getX() * (MIN_SCALE - node
											.getScale())), nbounds.getY()
									- (node.getBoundsReference().getCenter2D()
											.getX() * (MIN_SCALE - node
											.getScale())), MIN_SCALE, 0d,
							ANIMATION_DUR);
			a.setDelegate(new PActivity.PActivityDelegate() {
				public void activityStarted(PActivity activity) {
					animatingNodes.add(node);
				}

				public void activityStepped(PActivity activity) {
					if (node.getScale() < getFisheyeScaleFactor(node,
							lastFisheye)) {
						ta.terminate();
						a.terminate(PActivity.TERMINATE_WITHOUT_FINISHING);
						animatingNodes.remove(node);
					}
				}

				public void activityFinished(PActivity activity) {
					animatingNodes.remove(node);
				}
			});
		}
		formerCollection = selection;
	}

	public static void main(String[] args) {
		new FisheyeToyDemo();
	}
}
