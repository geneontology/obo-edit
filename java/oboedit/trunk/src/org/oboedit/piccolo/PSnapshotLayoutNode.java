package org.oboedit.piccolo;

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
	protected Map<PNode, PNode> objectToCloneMap;
	protected Map<PNode, PNode> cloneToObjectMap;
	protected List<PNodeRestorer> stateRestorers = new LinkedList<PNodeRestorer>();
	protected ShapeExtender shapeExtender = new EndpointShapeExtender();

	public PSnapshotLayoutNode() {
		stateRestorers.add(new DefaultNodeRestorer());
	}

	public void takeSnapshot() {
		objectToCloneMap = new HashMap<PNode, PNode>();
		cloneToObjectMap = new HashMap<PNode, PNode>();
		cloneAndFile(this);
	}

	protected PActivity animateRestoreState(final PNode node,
			final PNode clone, long duration, boolean currentState) {
		final Collection<PNode> preAnimateNodeChildren = 
			new LinkedList<PNode>(node.getChildrenReference());
		final Collection<PNode> postAnimateKillList = new LinkedList<PNode>();
		if (node == null) {
			preAnimateNodeChildren.add(clone);
			postAnimateKillList.add(clone);
		}
		PCompoundActivity out = new PCompoundActivity();

		Iterator<PNodeRestorer> itr = stateRestorers.iterator();
		while (itr.hasNext()) {
			PNodeRestorer restorer = itr.next();
			PActivity a;
			if (currentState)
				a = restorer.animateRestoreState(node, node, duration);
			else
				a = restorer.animateRestoreState(node, clone, duration);

			if (a != null)
				out.addActivity(a);
		}
		if (node != null && clone != null) {
			Collection<PNode> nodeChildren = new LinkedList(preAnimateNodeChildren);
			Iterator it = clone.getChildrenIterator();
			while (it.hasNext()) {
				PNode cloneChild = (PNode) it.next();
				PNode realChild = cloneToObjectMap.get(cloneChild);
				out.addActivity(animateRestoreState(realChild, cloneChild,
						duration, currentState));
				nodeChildren.remove(realChild);
			}
			Iterator<PNode> itp = nodeChildren.iterator();
			while (itp.hasNext()) {
				PNode realChild = itp.next();
				out.addActivity(animateRestoreState(realChild, null, duration, currentState));
			}
		}
		out.setDelegate(new PActivityDelegate() {

			public void activityFinished(PActivity activity) {
				Iterator<PNode> it = postAnimateKillList.iterator();
				while (it.hasNext()) {
					PNode child = it.next();
					if (node.getChildrenReference().contains(child))
						node.removeChild(child);

				}
			}

			public void activityStarted(PActivity activity) {
				node.removeAllChildren();
				Iterator<PNode> it = preAnimateNodeChildren.iterator();
				while (it.hasNext()) {
					PNode child = it.next();
					node.addChild(child);
					logger.info("adding child " + child + " to " + node);
				}
			}

			public void activityStepped(PActivity activity) {
			}

		});

		return out;
	}

	public void restoreState(PNode node, PNode clone) {
		Iterator<PNodeRestorer> itr = stateRestorers.iterator();

		while (itr.hasNext()) {
			PNodeRestorer restorer = itr.next();
			restorer.restoreState(node, clone);
		}
		Collection<PNode> nodeChildren = new LinkedList<PNode>(node.getChildrenReference());
		node.removeAllChildren();
		Iterator<PNode> it = clone.getChildrenIterator();
		while (it.hasNext()) {
			PNode cloneChild = it.next();
			PNode realChild = cloneToObjectMap.get(cloneChild);
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
			PNode child = it.next();
			if (node.getChildrenReference().contains(child))
				node.removeChild(child);
		}
	}

	public void restoreState() {
		PNode node = this;
		PNode clone = objectToCloneMap.get(node);
		restoreState(node, clone);
		objectToCloneMap = null;
		cloneToObjectMap = null;
	}

	public PActivity getAnimationToSnapshotState(long duration) {
		PNode node = this;
		PNode clone = objectToCloneMap.get(node);
		PActivity out = animateRestoreState(node, clone, duration, false);
		return out;
	}

	public PActivity getAnimationToCurrentState(long duration) {
		PNode node = this;
		PNode clone = objectToCloneMap.get(node);
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
		List<PNode> childList = new LinkedList<PNode>(node.getChildrenReference());
		PNode clone = (PNode) node.clone();
		clone.removeAllChildren();

		Iterator<PNode> it = childList.iterator();
		for (int i = 0; it.hasNext(); i++) {
			PNode child = it.next();
			clone.addChild(cloneAndFile(child));
		}
		objectToCloneMap.put(node, clone);
		cloneToObjectMap.put(clone, node);
		return clone;
	}
}
