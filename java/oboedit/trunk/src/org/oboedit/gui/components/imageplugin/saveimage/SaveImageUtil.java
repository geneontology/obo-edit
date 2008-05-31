package org.oboedit.gui.components.imageplugin.saveimage;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.framework.IOManager;

public class SaveImageUtil {

	public static void savePainter(ImagePainter painter) {
		try {
			IOManager.getManager().doOperation(AbstractGraphicsAdapter.STORE_IMAGE,
					painter, true);
		} catch (DataAdapterException e) {
			e.printStackTrace();
		}
	}

}
