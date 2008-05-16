package org.oboedit.graph;

import java.awt.Color;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;

import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.activities.PInterpolatingActivity;
import edu.umd.cs.piccolo.nodes.PPath;

import org.apache.log4j.*;

public class SpinnerBehavior implements ViewBehavior, NodeDecorator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SpinnerBehavior.class);

	protected LinkDatabaseCanvas canvas;

	public void install(LinkDatabaseCanvas canvas) {
		canvas.addDecorator(this);
		this.canvas = canvas;
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeDecorator(this);
		this.canvas = null;
	}

	public PActivity decorate(final PNode node, boolean noAnimation) {
		if (!noAnimation) {
			Area s = new Area();
			s.add(new Area(new Ellipse2D.Double(0, 100, 300,
					100)));
			s.add(new Area(new Ellipse2D.Double(100, 0, 100,
					300)));
			final PPath circleOne = new PPath(s);
			circleOne.setPaint(Color.red);
			circleOne.setStrokePaint(null);
			circleOne.setTransparency(.5f);

			Point2D p = new Point2D.Double((node.getFullBoundsReference()
					.getWidth() - 300) / 2, (node.getFullBoundsReference()
					.getHeight() - 300) / 2);

			circleOne.setOffset(p);
			node.addChild(circleOne);
			circleOne.moveToBack();
			PInterpolatingActivity a1 = (PInterpolatingActivity) PiccoloUtil
					.animateRotateAboutPoint(circleOne, 150, 150,
							Math.PI / 2, 1000);
			a1.setSlowInSlowOut(false);
			a1.setLoopCount(Integer.MAX_VALUE);
			return a1;
		} else
			return null;
	}

	public boolean onlyDecorateAfterLayout() {
		return false;
	}

}
