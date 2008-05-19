package org.oboedit.piccolo;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.bbop.swing.ShapeExtender;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.nodes.PPath;

import org.apache.log4j.*;

public class PageLayoutAnimator extends PLayoutNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PageLayoutAnimator.class);
	protected Map objectToCloneMap = new HashMap();
	protected Map cloneToObjectMap = new HashMap();
	
	public void addChild(PNode node) {
		PNode clone = cloneAndFile(node);
		logger.info("node = "+node+", clone = "+clone+", clone == node is "+(clone == node));
		super.addChild(clone);
	}
	
	public PNode getSurrogate(PNode original) {
		return (PNode) objectToCloneMap.get(original);
	}
	
	protected PNode cloneAndFile(PNode node) {
		List childList = new LinkedList(node.getChildrenReference());
		PNode clone = (PNode) node.clone();
		clone.removeAllChildren();
		
		Iterator it = childList.iterator();
		for(int i=0; it.hasNext(); i++) {
			PNode child = (PNode) it.next();
			clone.addChild(cloneAndFile(child));
		}
		objectToCloneMap.put(node, clone);
		cloneToObjectMap.put(clone, node);
		return clone;
	}
	
	public PActivity animateToLayout(Collection objects, ShapeExtender shapeExtender, long duration) {
		layoutChildren();
		Point2D myOffset = getOffset();
		setOffset(0,0);
		PCompoundActivity out = new PCompoundActivity();
		Iterator it = objects.iterator();
		while(it.hasNext()) {
			PNode node = (PNode) it.next();
			PNode clone = (PNode) objectToCloneMap.get(node);
			Point2D newCoords = globalToLocal(clone.getGlobalTranslation());
			double newScale = clone.getGlobalScale() / getGlobalScale();
			double newRotate = clone.getGlobalRotation() - getGlobalRotation();
			out.addActivity(node.animateToPositionScaleRotation(newCoords.getX()+myOffset.getX(), newCoords.getY()+myOffset.getY(), newScale, newRotate, duration));
			out.addActivity(node.animateToTransparency(clone.getTransparency(), duration));
			if (node instanceof PPath) {
				out.addActivity(new MorphActivity((PPath) node, ((PPath) clone).getPathReference(), shapeExtender, duration));
			}
		}
		setOffset(myOffset);
		return out;
	}
}
