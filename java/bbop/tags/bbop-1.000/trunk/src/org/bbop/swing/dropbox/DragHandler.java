package org.bbop.swing.dropbox;

import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.TransferHandler;
import javax.swing.event.MouseInputAdapter;

public class DragHandler extends MouseInputAdapter {
	protected MouseEvent firstMouseEvent;
	protected DropBoxContents contents;
	
	public DragHandler(DropBoxContents contents) {
		this.contents = contents;
	}

	public void mousePressed(MouseEvent e) {
		firstMouseEvent = e;
	}

	public void mouseDragged(MouseEvent e) {
		if (firstMouseEvent != null) {
			JComponent c = (JComponent) e.getComponent();
			TransferHandler handler = c.getTransferHandler();
			// Tell the transfer handler to initiate the drag.
			handler.exportAsDrag(c, firstMouseEvent, TransferHandler.COPY);
			System.err.println("exporting drag "+e);
			firstMouseEvent = null;
		}
	}

	public void mouseReleased(MouseEvent e) {
		firstMouseEvent = null;
	}
}