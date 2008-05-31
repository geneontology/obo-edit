package org.oboedit.gui.components.imageplugin.saveimage;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;

import org.bbop.dataadapter.DataAdapterException;

public class ImageAdapter extends AbstractGraphicsAdapter {
	
	protected String formatName;
	protected String formatID;
	protected BufferedImage image;
	
	public ImageAdapter(String formatID, String name) {
		this.formatID = formatID;
		this.formatName = name;
	}

	@Override
	protected void flushGraphics(Graphics2D g, String path)
			throws DataAdapterException {
		try {
			ImageIO.write(image, formatID, new File(path));
		} catch (IOException e) {
			throw new DataAdapterException(e);
		}
	}

	@Override
	protected Graphics2D getGraphics(String path, int width, int height)
			throws DataAdapterException {
		image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		image.getGraphics().setColor(Color.white);
		image.getGraphics().fillRect(0, 0, width, height);		
		return image.createGraphics();
	}

	public String getID() {
		return formatID;
	}

	public String getName() {
		return formatName;
	}

}
