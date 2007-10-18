package org.oboedit.graph;

import java.awt.Color;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.PathCapable;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;

public class FocusPicker implements ViewBehavior, NodeDecorator {

	protected LinkDatabaseCanvas canvas;

	protected PBasicInputEventHandler handler = new PBasicInputEventHandler() {
		@Override
		public void mousePressed(PInputEvent event) {
			if (event.getClickCount() == 1) {
				PCNode node = (PCNode) PiccoloUtil.getNodeOfClass(event
						.getPath(), PCNode.class);
				if (node != null) {
					if (node instanceof OENode)
						System.err.println("node dim = " + node.getWidth()
								+ "," + node.getHeight() + ", prefWidth = "
								+ ((OENode) node).getPreferredWidth());
					if (!ObjectUtil.equals(canvas.getFocusedNode(), node)) {
						canvas.setFocusedObject(node.getObject());
					}
					return;
				}
			}
		}
	};

	protected FocusedNodeListener focusListener = new FocusedNodeListener() {
		public void focusedChanged(PathCapable oldFocus, PathCapable newFocus) {
			decorate(oldFocus, true);
			decorate(newFocus, true);
		}
	};

	public void install(LinkDatabaseCanvas canvas) {
		canvas.addInputEventListener(handler);
		canvas.addDecorator(this);
		canvas.addFocusedNodeListener(focusListener);
		this.canvas = canvas;
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeInputEventListener(handler);
		canvas.removeDecorator(this);
		canvas.removeFocusedNodeListener(focusListener);
		this.canvas = null;
	}

	public PActivity decorate(PathCapable pc, boolean noAnimation) {
		PCNode node = (PCNode) canvas.getNode(pc);
		if (node == null)
			return null;
		else
			return decorate(node, noAnimation);
	}

	public PActivity decorate(PNode node, boolean noAnimation) {
		if (!(node instanceof OENode))
			return null;
		if (canvas.getFocusedNode() != null
				&& canvas.getFocusedNode().equals(node))
			node.setPaint(Color.white);
		else
			node.setPaint(Color.lightGray);
		return null;
	}

	public boolean onlyDecorateAfterLayout() {
		return false;
	}

}
