package org.oboedit.graph;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

public class LinkoutMeterBehavior implements ViewBehavior, NodeDecorator {
	public static final Object CHILD_METER = new Object();

	public static final Object PARENT_METER = new Object();
	
	protected HistoryListener historyListener = new HistoryListener() {

		public void applied(HistoryAppliedEvent event) {
			rebuildNodes();
		}

		public void reversed(HistoryAppliedEvent event) {
			rebuildNodes();
		}

	};
	
	protected RelayoutListener relayoutListener = new RelayoutListener() {

		public void relayoutComplete() {
			rebuildNodes();
		}

		public void relayoutStarting() {
		}
		
	};
	
	public LinkoutMeterBehavior() {
	}

	protected LinkDatabaseCanvas canvas;
	
	protected void rebuildNodes() {
		for(PathCapable lo : canvas.getVisibleObjects()) {
			if (lo instanceof LinkedObject) {
				OENode node = (OENode) canvas.getNode(lo);
				if (node == null)
					continue;
				LinkoutMeter childLinkoutMeter = (LinkoutMeter) node.getNamedChild(CHILD_METER);
				if (childLinkoutMeter != null)
					childLinkoutMeter.rebuildParentList();
				LinkoutMeter parentLinkoutMeter = (LinkoutMeter) node.getNamedChild(PARENT_METER);
				if (parentLinkoutMeter != null)
					parentLinkoutMeter.rebuildParentList();				
			}
		}
	}
	
	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addDecorator(this);
		canvas.addRelayoutListener(relayoutListener);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeDecorator(this);
		canvas.removeRelayoutListener(relayoutListener);
		this.canvas = null;
	}

	public PActivity decorate(PNode n, boolean noAnimation) {
		if (n instanceof OENode) {
			OENode node = (OENode) n;
			LinkoutMeter childLinkoutMeter = (LinkoutMeter) node.getNamedChild(CHILD_METER);
			LinkoutMeter parentLinkoutMeter = (LinkoutMeter) node.getNamedChild(PARENT_METER);
			if (childLinkoutMeter == null && parentLinkoutMeter == null) {
				childLinkoutMeter = new LinkoutMeter((LinkedObject) node.getObject(), canvas, true);
				parentLinkoutMeter = new LinkoutMeter((LinkedObject) node.getObject(), canvas, false);
				node.setNamedChild(CHILD_METER, childLinkoutMeter);
				node.setNamedChild(PARENT_METER, parentLinkoutMeter);
				PiccoloUtil.centerInParent(childLinkoutMeter, true, false, true);
				PiccoloUtil.centerInParent(parentLinkoutMeter, true, false, true);
				childLinkoutMeter.setOffset(childLinkoutMeter.getXOffset(),
						node.getFullBoundsReference().getHeight() - childLinkoutMeter.getHeight()*2);

			}
		}
		return null;
	}

	public boolean onlyDecorateAfterLayout() {
		return true;
	}

}
