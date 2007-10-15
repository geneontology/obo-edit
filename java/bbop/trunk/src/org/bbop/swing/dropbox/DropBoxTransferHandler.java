package org.bbop.swing.dropbox;

import java.awt.Component;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

public class DropBoxTransferHandler extends TransferHandler {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1195744937397730938L;

	protected DragHandler selectorDragHandler;
	protected DropBoxContents sourceSelector;
	protected Component handle;

	public DropBoxTransferHandler(DropBoxContents c, Component handle) {
		sourceSelector = c;
		this.handle = handle;
		selectorDragHandler = new DragHandler(c);
	}
	
	public Component getHandle() {
		return handle;
	}

	public static DropBoxTransferHandler installHandler(DropBoxContents c, Component handle) {
		DropBoxTransferHandler handler = new DropBoxTransferHandler(c, handle);
		((JComponent) handle).setTransferHandler(handler);
		handle.addMouseMotionListener(handler
				.getDropBoxDragHandler());
		handle.addMouseListener(handler.getDropBoxDragHandler());
		return handler;
	}

	public DropBoxContents getObjectSelector() {
		return sourceSelector;
	}

	public static void removeHandler(DropBoxTransferHandler handler) {
		((JComponent) handler.getHandle()).setTransferHandler(null);
		((JComponent) handler.getHandle())
				.removeMouseMotionListener(handler.getDropBoxDragHandler());
		((JComponent) handler.getHandle()).removeMouseListener(handler
				.getDropBoxDragHandler());
	}

	public DragHandler getDropBoxDragHandler() {
		return selectorDragHandler;
	}

	public boolean importData(JComponent c, Transferable t) {
		if (!(c instanceof DropBoxPanel))
			return false;
		if (canImport(c, t.getTransferDataFlavors())) {
			DropBoxPanel droppable = (DropBoxPanel) c;

			try {
				sourceSelector = (DropBoxContents) t.getTransferData(DropBoxContentsTransferable.DROP_BOX_CONTENTS_FLAVOR);
				droppable.add((Component) sourceSelector);
				return true;
			} catch (UnsupportedFlavorException ufe) {
				System.out.println("importData: unsupported data flavor");
			} catch (IOException ioe) {
				System.out.println("importData: I/O exception");
			}
		}
		return false;
	}

	protected Transferable createTransferable(JComponent c) {
		return new DropBoxContentsTransferable(sourceSelector);
	}

	public int getSourceActions(JComponent c) {
		return COPY_OR_MOVE;
	}

	public boolean canImport(JComponent c, DataFlavor[] flavors) {
		for (int i = 0; i < flavors.length; i++) {
			if (DropBoxContentsTransferable.DROP_BOX_CONTENTS_FLAVOR.equals(flavors[i])) {
				return true;
			}
		}
		return false;
	}
}

