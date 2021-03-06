package org.oboedit.piccolo;

import java.awt.geom.Point2D;

import org.bbop.swing.EndpointShapeExtender;
import org.bbop.swing.ShapeExtender;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PInterpolatingActivity;
import edu.umd.cs.piccolo.nodes.PPath;

public class DefaultMorpher implements Morpher {

	protected PNode newNodeOriginNode = null;

	protected PNode deadNodeDestNode = null;

	protected PNode deadNodeOriginNode = null;

	protected PNode newNodeDestNode = null;

	protected ShapeExtender extender = new EndpointShapeExtender();

	protected Point2D scratchPoint = new Point2D.Double();

	public PCompoundActivity morph(PNode oldNode, PNode newNode, long duration) {
		PCompoundActivity relayoutActivity = new PCompoundActivity();
		if (newNode == null) {
			if (deadNodeDestNode != null) {
				scratchPoint.setLocation(deadNodeDestNode.getX()
						- deadNodeDestNode.getFullBoundsReference().getWidth()
						/ 2, deadNodeDestNode.getY()
						- deadNodeDestNode.getFullBoundsReference().getHeight()
						/ 2);
				PInterpolatingActivity effect = oldNode
						.animateToPositionScaleRotation(scratchPoint.getX(),
								scratchPoint.getY(), .001, 0, duration);
				relayoutActivity.addActivity(effect);
			} else {
				PInterpolatingActivity effect = oldNode.animateToTransparency(
						0, duration);
				relayoutActivity.addActivity(effect);
			}
		} else if (oldNode == null) {
			if (newNodeOriginNode != null) {

				scratchPoint.setLocation(newNodeOriginNode.getXOffset()
						- newNodeOriginNode.getFullBoundsReference().getWidth()
						/ 2, newNodeOriginNode.getYOffset()
						- newNodeOriginNode.getFullBoundsReference().getHeight()
						/ 2);
				PInterpolatingActivity effect = newNode
						.animateToPositionScaleRotation(newNode.getXOffset(),
								newNode.getYOffset(), 1, 0, duration);
				newNode.setScale(.01);
				newNode.setOffset(scratchPoint);
				newNode.moveToBack();
				relayoutActivity.addActivity(effect);
			} else {
				newNode.setTransparency(0);
				PInterpolatingActivity effect = newNode.animateToTransparency(
						1, duration);
				relayoutActivity.addActivity(effect);
			}
		} else {
			if (newNode instanceof PPath && oldNode instanceof PPath) {
				relayoutActivity.addActivity(new MorphActivity((PPath) oldNode,
						((PPath) newNode).getPathReference(), extender,
						duration));
			} else {
				/*
				 * float newTrans = newNode.getTransparency();
				 * newNode.setTransparency(0);
				 * relayoutActivity.addActivity(oldNode.animateToTransparency(0,
				 * duration));
				 * relayoutActivity.addActivity(newNode.animateToTransparency(
				 * newTrans, duration));
				 */
			}
			relayoutActivity.addActivity(oldNode
					.animateToPositionScaleRotation(newNode.getXOffset(),
							newNode.getYOffset(), newNode.getScale(), newNode
									.getRotation(), duration));
		}
		return relayoutActivity;
	}

	public ShapeExtender getExtender() {
		return extender;
	}

	public void setExtender(ShapeExtender extender) {
		this.extender = extender;
	}

	public PNode getDeadNodeDestNode() {
		return deadNodeDestNode;
	}

	public void setDeadNodeDestNode(PNode deadNodeDestNode) {
		this.deadNodeDestNode = deadNodeDestNode;
	}

	public PNode getNewNodeOriginNode() {
		return newNodeOriginNode;
	}

	public void setNewNodeOriginNode(PNode newNodeOriginNode) {
		this.newNodeOriginNode = newNodeOriginNode;
	}

	public PNode getDeadNodeOriginNode() {
		return deadNodeOriginNode;
	}

	public void setDeadNodeOriginNode(PNode deadNodeOriginNode) {
		this.deadNodeOriginNode = deadNodeOriginNode;
	}

	public PNode getNewNodeDestNode() {
		return newNodeDestNode;
	}

	public void setNewNodeDestNode(PNode newNodeDestNode) {
		this.newNodeDestNode = newNodeDestNode;
	}

}
