package org.oboedit.graph;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.TransferHandler;

import org.obo.datamodel.PathCapable;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.SelectionTransferHandler;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;

import org.apache.log4j.*;

public class PickToolButtonBehavior implements ToolbarButtonBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PickToolButtonBehavior.class);
	
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

	protected PInputEventListener selectionMouseListener;
	
	protected LinkDatabaseCanvas canvas;

	public void activate(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		selectionMouseListener = new SelectionMouseListener(canvas);
		canvas.setTransferHandler(new SelectionTransferHandler(canvas));
		canvas.addInputEventListener(dragAndDropGestureListener);
		canvas.addInputEventListener(selectionMouseListener);
	}

	public void deactivate(LinkDatabaseCanvas canvas) {
		canvas.setTransferHandler(null);
		canvas.removeInputEventListener(dragAndDropGestureListener);
		canvas.removeInputEventListener(selectionMouseListener);
		logger.info("Uninstalled drag & drop behavior");
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
