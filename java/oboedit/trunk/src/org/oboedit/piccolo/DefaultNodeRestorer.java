package org.oboedit.piccolo;

import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.bbop.swing.EndpointShapeExtender;
import org.bbop.swing.ShapeExtender;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.nodes.PPath;

import org.apache.log4j.*;

public class DefaultNodeRestorer implements PNodeRestorer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultNodeRestorer.class);

	protected ShapeExtender shapeExtender = new EndpointShapeExtender();

	public PActivity animateRestoreState(PNode fromState, PNode toState, long duration) {
		if (fromState == null || toState == null)
			return null;
		PCompoundActivity out = new PCompoundActivity();

		out.addActivity(new PositionScaleRotationActivity(fromState, toState.getXOffset(),
				toState.getYOffset(), toState.getScale(), toState.getRotation(),
				duration));
		out.addActivity(fromState.animateToTransparency(toState.getTransparency(),
				duration));
		if (fromState instanceof PPath) {
			out.addActivity(new MorphActivity((PPath) fromState, ((PPath) toState)
					.getPathReference(), shapeExtender, duration));
		}
		return out;
	}

	public void cleanup() {
	}

	public void init() {
	}

	public void restoreState(PNode node, PNode clone) {
		node.setOffset(clone.getOffset());
		node.setRotation(clone.getRotation());
		node.setScale(clone.getScale());
		node.setTransparency(clone.getTransparency());
		if (node instanceof PPath) {
			((PPath) node).setPathTo(((PPath) clone).getPathReference());
		}
	}

	public ShapeExtender getShapeExtender() {
		return shapeExtender;
	}

	public void setShapeExtender(ShapeExtender shapeExtender) {
		this.shapeExtender = shapeExtender;
	}

}
