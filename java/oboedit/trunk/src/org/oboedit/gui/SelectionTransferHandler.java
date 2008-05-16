package org.oboedit.gui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

import org.oboedit.gui.event.SelectorDragHandler;

import org.apache.log4j.*;

public class SelectionTransferHandler extends TransferHandler {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SelectionTransferHandler.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1195744937397730938L;

	public static DataFlavor SELECTION_FLAVOR = new DataFlavor(
			Selection.class, "Selection");

	protected ObjectSelector sourceSelector;

	protected Selection selection;

	protected SelectorDragHandler selectorDragHandler;

	public SelectionTransferHandler(ObjectSelector c) {
		sourceSelector = c;
		selectorDragHandler = new SelectorDragHandler(c);
	}

	public static SelectionTransferHandler installHandler(ObjectSelector c) {
		SelectionTransferHandler handler = new SelectionTransferHandler(c);
		((JComponent) c).setTransferHandler(handler);
		((JComponent) c).addMouseMotionListener(handler
				.getSelectorDragHandler());
		((JComponent) c).addMouseListener(handler.getSelectorDragHandler());
		return handler;
	}

	public ObjectSelector getObjectSelector() {
		return sourceSelector;
	}

	public static void removeHandler(SelectionTransferHandler handler) {
		((JComponent) handler.getObjectSelector()).setTransferHandler(null);
		((JComponent) handler.getObjectSelector())
				.removeMouseMotionListener(handler.getSelectorDragHandler());
		((JComponent) handler.getObjectSelector()).removeMouseListener(handler
				.getSelectorDragHandler());
	}

	public SelectorDragHandler getSelectorDragHandler() {
		return selectorDragHandler;
	}

	public boolean importData(JComponent c, Transferable t) {
		if (!(c instanceof ObjectSelector))
			return false;
		if (canImport(c, t.getTransferDataFlavors())) {
			SelectionDroppable droppable = (SelectionDroppable) c;

			try {
				selection = (Selection) t.getTransferData(SELECTION_FLAVOR);
				droppable.drop(selection);
				return true;
			} catch (UnsupportedFlavorException ufe) {
				logger.info("importData: unsupported data flavor");
			} catch (IOException ioe) {
				logger.info("importData: I/O exception");
			}
		}
		return false;
	}

	protected Transferable createTransferable(JComponent c) {
		return new SelectionTransferable((ObjectSelector) c);
	}

	public int getSourceActions(JComponent c) {
		return COPY_OR_MOVE;
	}

	public boolean canImport(JComponent c, DataFlavor[] flavors) {
		for (int i = 0; i < flavors.length; i++) {
			if (SELECTION_FLAVOR.equals(flavors[i])) {
				return true;
			}
		}
		return false;
	}

	public static class SelectionTransferable implements Transferable {
		private Selection selection;

		public SelectionTransferable(ObjectSelector selector) {
			selection = selector.getSelection();
		}

		public Object getTransferData(DataFlavor flavor)
				throws UnsupportedFlavorException {
			if (!isDataFlavorSupported(flavor)) {
				throw new UnsupportedFlavorException(flavor);
			}
			return selection;
		}

		public DataFlavor[] getTransferDataFlavors() {
			return new DataFlavor[] { SELECTION_FLAVOR };
		}

		public boolean isDataFlavorSupported(DataFlavor flavor) {
			return SELECTION_FLAVOR.equals(flavor);
		}
	}
}
