package org.oboedit.piccolo;

import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.swing.EndpointShapeExtender;
import org.bbop.swing.ShapeExtender;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.activities.PActivity.PActivityDelegate;

/**
 * A layout node that can remember a snapshot of its state, be changed, and then
 * animate from the snapshot state to the current state
 * 
 * @author jrichter
 * 
 */
import org.apache.log4j.*;

public class PSnapshotLayoutNode extends PLayoutNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PSnapshotLayoutNode.class);
	protected Map objectToCloneMap;
	protected Map cloneToObjectMap;
	protected List stateRestorers = new LinkedList();
	protected ShapeExtender shapeExtender = new EndpointShapeExtender();

	public PSnapshotLayoutNode() {
		stateRestorers.add(new DefaultNodeRestorer());
	}

	public void takeSnapshot() {
		objectToCloneMap = new HashMap();
		cloneToObjectMap = new HashMap();
		cloneAndFile(this);
	}

	protected PActivity animateRestoreState(final PNode node,
			final PNode clone, long duration, boolean currentState) {
		final Collection preAnimateNodeChildren = new LinkedList(node
				.getChildrenReference());
		final Collection postAnimateKillList = new LinkedList();
		if (node == null) {
			preAnimateNodeChildren.add(clone);
			postAnimateKillList.add(clone);
		}
		PCompoundActivity out = new PCompoundActivity();

		Iterator it = stateRestorers.iterator();
		while (it.hasNext()) {
			PNodeRestorer restorer = (PNodeRestorer) it.next();
			PActivity a;
			if (currentState)
				a = restorer.animateRestoreState(node, node, duration);
			else
				a = restorer.animateRestoreState(node, clone, duration);

			if (a != null)
				out.addActivity(a);
		}
		if (node != null && clone != null) {
			Collection nodeChildren = new LinkedList(preAnimateNodeChildren);
			it = clone.getChildrenIterator();
			while (it.hasNext()) {
				PNode cloneChild = (PNode) it.next();
				PNode realChild = (PNode) cloneToObjectMap.get(cloneChild);
				out.addActivity(animateRestoreState(realChild, cloneChild,
						duration, currentState));
				nodeChildren.remove(realChild);
			}
			it = nodeChildren.iterator();
			while (it.hasNext()) {
				PNode realChild = (PNode) it.next();
				out.addActivity(animateRestoreState(realChild, null, duration, currentState));
			}
		}
		out.setDelegate(new PActivityDelegate() {

			public void activityFinished(PActivity activity) {
				Iterator it = postAnimateKillList.iterator();
				while (it.hasNext()) {
					PNode child = (PNode) it.next();
					if (node.getChildrenReference().contains(child))
						node.removeChild(child);

				}
			}

			public void activityStarted(PActivity activity) {
				node.removeAllChildren();
				Iterator it = preAnimateNodeChildren.iterator();
				while (it.hasNext()) {
					PNode child = (PNode) it.next();
					node.addChild(child);
					logger.error("adding child " + child + " to " + node);
				}
			}

			public void activityStepped(PActivity activity) {
			}

		});

		return out;
	}

	public void restoreState(PNode node, PNode clone) {
		Iterator it = stateRestorers.iterator();

		while (it.hasNext()) {
			PNodeRestorer restorer = (PNodeRestorer) it.next();
			restorer.restoreState(node, clone);
		}
		Collection nodeChildren = new LinkedList(node.getChildrenReference());
		node.removeAllChildren();
		it = clone.getChildrenIterator();
		while (it.hasNext()) {
			PNode cloneChild = (PNode) it.next();
			PNode realChild = (PNode) cloneToObjectMap.get(cloneChild);
			if (realChild == null) {
				node.addChild(cloneChild);
			} else {
				restoreState(realChild, cloneChild);
				node.addChild(realChild);
				nodeChildren.remove(realChild);

			}
		}
		it = nodeChildren.iterator();
		while (it.hasNext()) {
			PNode child = (PNode) it.next();
			if (node.getChildrenReference().contains(child))
				node.removeChild(child);
		}
	}

	public void restoreState() {
		PNode node = this;
		PNode clone = (PNode) objectToCloneMap.get(node);
		restoreState(node, clone);
		objectToCloneMap = null;
		cloneToObjectMap = null;
	}

	public PActivity getAnimationToSnapshotState(long duration) {
		PNode node = this;
		PNode clone = (PNode) objectToCloneMap.get(node);
		PActivity out = animateRestoreState(node, clone, duration, false);
		return out;
	}

	public PActivity getAnimationToCurrentState(long duration) {
		PNode node = this;
		PNode clone = (PNode) objectToCloneMap.get(node);
		PActivity out = animateRestoreState(node, clone, duration, true);
		return out;
	}

	public void addStateRestorer(PNodeRestorer restorer) {
		stateRestorers.add(restorer);
	}

	public void removeStateRestorer(PNodeRestorer restorer) {
		stateRestorers.remove(restorer);
	}

	protected PNode cloneAndFile(PNode node) {
		List childList = new LinkedList(node.getChildrenReference());
		PNode clone = (PNode) node.clone();
		clone.removeAllChildren();

		Iterator it = childList.iterator();
		for (int i = 0; it.hasNext(); i++) {
			PNode child = (PNode) it.next();
			clone.addChild(cloneAndFile(child));
		}
		objectToCloneMap.put(node, clone);
		cloneToObjectMap.put(clone, node);
		return clone;
	}
}
