package org.bbop.swing.dropbox;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;

public class DropBoxContentsTransferable implements Transferable {
	private DropBoxContents selection;

	public static DataFlavor DROP_BOX_CONTENTS_FLAVOR = new DataFlavor(
			DropBoxContents.class, "DropBoxContents");

	public DropBoxContentsTransferable(DropBoxContents selector) {
		selection = selector;
	}

	public Object getTransferData(DataFlavor flavor)
			throws UnsupportedFlavorException {
		if (!isDataFlavorSupported(flavor)) {
			throw new UnsupportedFlavorException(flavor);
		}
		return selection;
	}

	public DataFlavor[] getTransferDataFlavors() {
		return new DataFlavor[] { DROP_BOX_CONTENTS_FLAVOR };
	}

	public boolean isDataFlavorSupported(DataFlavor flavor) {
		return DROP_BOX_CONTENTS_FLAVOR.equals(flavor);
	}
}
