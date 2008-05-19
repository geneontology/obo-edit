package org.bbop.swing.dropbox;

import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.TransferHandler;
import javax.swing.event.MouseInputAdapter;

import org.apache.log4j.*;

public class DragHandler extends MouseInputAdapter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DragHandler.class);
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
			logger.info("exporting drag "+e);
			firstMouseEvent = null;
		}
	}

	public void mouseReleased(MouseEvent e) {
		firstMouseEvent = null;
	}
}
