package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Cursor;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.TransferHandler;

import org.obo.datamodel.PathCapable;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.SelectionTransferHandler;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;
import edu.umd.cs.piccolo.util.PPickPath;

public class PickToolButtonBehavior implements ToolbarButtonBehavior {
	protected static final Object MARQUEE_KEY = new Object();
	
	protected PBasicInputEventHandler dragAndDropGestureListener = new PBasicInputEventHandler() {
		PInputEvent firstMouseEvent;

		public void mousePressed(PInputEvent e) {
			if (canvas.getSelection().isEmpty() || !e.isLeftMouseButton())
				return;

			firstMouseEvent = e;
		}

		public void mouseDragged(PInputEvent e) {
			if (canvas.getSelection().isEmpty() || !e.isLeftMouseButton())
				return;
			boolean inSelection = false;
			for (PathCapable pc : canvas.getSelection().getAllSelectedObjects()) {
				PNode node = canvas.getNode(pc);
				if (node != null && node.equals(e.getPickedNode())) {
					inSelection = true;
					break;
				}
			}
			if (!inSelection)
				return;

			if (firstMouseEvent != null) {

				JComponent c = (JComponent) e.getComponent();
				TransferHandler handler = c.getTransferHandler();
				// Tell the transfer handler to initiate the drag.
				handler.exportAsDrag(c, firstMouseEvent.getSourceSwingEvent(),
						TransferHandler.COPY);
				firstMouseEvent = null;
			}
		}

		public void mouseReleased(PInputEvent e) {
			firstMouseEvent = null;
		}
	};

	protected PInputEventListener selectionMouseListener = new PBasicInputEventHandler() {

		protected Point2D startPoint = new Point2D.Double();

		@Override
		public void mousePressed(PInputEvent event) {
			if (event.isMiddleMouseButton()
					&& event.getClickCount() == 1) {
				startPoint.setLocation(event.getPosition());
			}
		}

		@Override
		public void mouseDragged(PInputEvent event) {
			if (event.isMiddleMouseButton()) {
				NamedChildProvider provider = canvas.getNamedChildProvider();
				PPath selectionMarquee = (PPath) provider.getNamedChild(
						MARQUEE_KEY, canvas.getLayer());

				if (selectionMarquee == null) {
					selectionMarquee = new PPath();
					selectionMarquee.setPickable(false);
					provider.setNamedChild(MARQUEE_KEY, canvas.getLayer(),
							selectionMarquee);
					selectionMarquee.setStrokePaint(Preferences
							.defaultSelectionColor());
					float[] dashes = { 5, 5 };
					selectionMarquee.setStroke(new BasicStroke(1,
							BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1f,
							dashes, 0f));
				}
				Rectangle2D rect = new Rectangle2D.Double();
				Point2D dragPos = event.getPosition();
				rect.setFrameFromDiagonal(startPoint, dragPos);
				selectionMarquee.setPathTo(rect);
			}
		}

		@Override
		public void mouseReleased(PInputEvent event) {
			if (event.isMiddleMouseButton()) {
				NamedChildProvider provider = canvas.getNamedChildProvider();
				PPath selectionMarquee = (PPath) provider.getNamedChild(
						MARQUEE_KEY, canvas.getLayer());
				if (selectionMarquee != null) {
					Collection<PathCapable> selected = new LinkedList<PathCapable>();
					Iterator it = canvas.getLayer().getAllNodes().iterator();
					while (it.hasNext()) {
						PNode child = (PNode) it.next();
						PBounds marqueeBounds = selectionMarquee
								.getGlobalBounds();
						if (marqueeBounds.intersects(child
								.getGlobalFullBounds())) {
							if (child instanceof OELink) {
								OELink oelink = (OELink) child;
								PNode iconPanel = oelink
										.getNamedChild(OELink.KEY_ICON_PANEL);
								if (iconPanel != null
										&& marqueeBounds.intersects(iconPanel
												.getGlobalFullBounds()))
									selected.add(oelink.getObject());
							} else if (child instanceof OENode) {
								selected.add(((OENode) child).getObject());
							}
						}
					}
					Selection selection = null;
					if (event.isShiftDown()) {
						selection = SelectionManager.addToSelection(canvas
								.getSelection(), selected, null, null, canvas
								.getRootAlgorithm(), canvas
								.getLinkProviderDatabase());
					} else if (event.isControlDown()) {
						selection = SelectionManager.removeFromSelection(canvas
								.getSelection(), selected);
					} else
						selection = SelectionManager.createSelection(canvas,
								selected, null, null,
								canvas.getRootAlgorithm(), canvas
										.getLinkProviderDatabase());
					provider
							.setNamedChild(MARQUEE_KEY, canvas.getLayer(), null);
					canvas.select(selection);
					canvas.decorate();
				}
			} else if (event.isLeftMouseButton()) {
				PPickPath path = canvas.getCamera().pick(
						event.getPosition().getX(), event.getPosition().getY(),
						1);
				PCNode node = (PCNode) PiccoloUtil.getNodeOfClass(event.getPath(),
						PCNode.class);
				if (node == null)
					return;
				
				if (node instanceof OENode && !event.getPickedNode().equals(node)) {
					return;
				}
					
				System.err.println("canvas kids = "
						+ canvas.getLayer().getChildrenReference());
				PathCapable pathCapable = node.getObject();
				Selection selection = null;
				if (event.isShiftDown()) {
					if (canvas.getSelection().getAllSelectedObjects().contains(
							pathCapable)) {
						selection = SelectionManager.removeFromSelection(canvas
								.getSelection(), pathCapable);
					} else {
						selection = SelectionManager.addToSelection(canvas
								.getSelection(), pathCapable, canvas
								.getRootAlgorithm(), canvas
								.getLinkProviderDatabase());
					}
				} else
					selection = SelectionManager.createSelection(canvas,
							pathCapable, canvas.getRootAlgorithm(), canvas
									.getLinkProviderDatabase());
				if (canvas.isLive()
						&& !SelectionManager.getManager()
								.doPreSelectValidation(canvas))
					return;
				
				canvas.select(selection);
				canvas.decorate();
			}

		}
	};
	
	protected LinkDatabaseCanvas canvas;

	public void activate(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.setTransferHandler(new SelectionTransferHandler(canvas));
		canvas.addInputEventListener(dragAndDropGestureListener);
		canvas.addInputEventListener(selectionMouseListener);
	}

	public void deactivate(LinkDatabaseCanvas canvas) {
		canvas.setTransferHandler(null);
		canvas.removeInputEventListener(dragAndDropGestureListener);
		canvas.removeInputEventListener(selectionMouseListener);
		System.err.println("Uninstalled drag & drop behavior");
		this.canvas = null;
	}

	public Icon getButtonIcon() {
		return Preferences.loadLibraryIcon("select_cursor.gif");
	}

	public String getTooltip() {
		return "<html><b>Drag and drop tool</b><br><hr>\n"
				+ "<table width=300><tr><td>Drag terms onto other terms to create new links,"
				+ "move terms or merge terms, or right-click for "
				+ "a popup menu of edits. Double-click or shift-double-click "
				+ "to modify the selection. Drag with the middle mouse "
				+ "button to rubber band select.</td></table>" + "</html>";
	}

	public String getButtonLabel() {
		return null;
	}

	public JComponent getConfigurationPanel() {
		return null;
	}
}
