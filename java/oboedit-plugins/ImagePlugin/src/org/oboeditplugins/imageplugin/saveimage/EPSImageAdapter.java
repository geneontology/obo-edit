package org.oboeditplugins.imageplugin.saveimage;

import java.awt.Graphics2D;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import org.bbop.dataadapter.DataAdapterException;
import org.sourceforge.jlibeps.epsgraphics.EpsGraphics2D;

public class EPSImageAdapter extends AbstractGraphicsAdapter {

	protected FileOutputStream finalImage;
	
	@Override
	protected void flushGraphics(Graphics2D gin, String path)
			throws DataAdapterException {
		EpsGraphics2D g = (EpsGraphics2D) gin;
		try {
			g.flush();
			g.close();
			finalImage.close();
		} catch (IOException e) {
			throw new DataAdapterException (e);
		}
	}

	@Override
	protected Graphics2D getGraphics(String path, int width, int height)
			throws DataAdapterException {
		try {
			File file = new File(path);
			finalImage = new FileOutputStream(file);
			EpsGraphics2D g = new EpsGraphics2D(file.getName(),
					finalImage, 0, 0, width, height);
			return g;
		} catch (IOException e) {
			throw new DataAdapterException (e);
		}
	}

	public String getID() {
		return "eps_image";
	}

	public String getName() {
		return "Encapsulated Post Script (EPS) Image";
	}

}
