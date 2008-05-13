package org.oboedit.gui.event;

import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.TransferHandler;
import javax.swing.event.MouseInputAdapter;

// import org.obo.datamodel.PathCapable;
import org.oboedit.gui.ObjectSelector;


public class SelectorDragHandler extends MouseInputAdapter {
	MouseEvent firstMouseEvent;
	protected ObjectSelector selector;
	
	public SelectorDragHandler(ObjectSelector selector) {
		this.selector = selector;
	}

	public void mousePressed(MouseEvent e) {
		// Don't bother to drag if there is no image.
		if (selector.getSelection().isEmpty())
			return;

		firstMouseEvent = e;
	}

	public void mouseDragged(MouseEvent e) {
//		System.err.println("SelectionDragHandler.mousedragged "+e +",\nfirstMouseEvent = " + firstMouseEvent);
		// Don't bother to drag if the component displays no image.
		if (selector.getSelection().isEmpty())
			return;
		// TODO Make it so selection drag can't initiate if the click doesn't occur
		// within the selection
		/*
		boolean inSelection = false;

		if (!inSelection)
			return;
			*/
//		System.err.println("SelectionDragHandler.mousedragged: c = " + c + ", selected objects = " + selector.getSelection().getAllSelectedObjects()); // DEL
//		if (!selector.getSelection().getAllSelectedObjects().contains(c))
//		    return;

		if (firstMouseEvent != null) {
			JComponent c = (JComponent) e.getComponent();
			TransferHandler handler = c.getTransferHandler();
			// Tell the transfer handler to initiate the drag.
			handler.exportAsDrag(c, firstMouseEvent, TransferHandler.COPY);
//			System.err.println("exporting drag "+e);
			firstMouseEvent = null;
		}
	}

	public void mouseReleased(MouseEvent e) {
		firstMouseEvent = null;
	}
}
