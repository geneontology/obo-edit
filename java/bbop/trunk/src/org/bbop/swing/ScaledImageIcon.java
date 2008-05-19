package org.bbop.swing;

import java.awt.*;
import javax.swing.*;

import org.apache.log4j.*;

public class ScaledImageIcon implements Icon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScaledImageIcon.class);

    protected Image image;
    protected int width;
    protected int height;

    public ScaledImageIcon() {
    }

    public ScaledImageIcon(Image image) {
	this(image, image.getWidth(null), image.getHeight(null));
    }

    public ScaledImageIcon(Image image, int width, int height) {
	setImage(image);
	setIconWidth(width);
	setIconHeight(height);
    }

    public int getIconWidth() {
	return width;
    }

    public int getIconHeight() {
	return height;
    }

    public void setImage(Image image) {
	this.image = image;
    }

    public void setIconWidth(int width) {
	this.width = width;
    }

    public void setIconHeight(int height) {
	this.height = height;
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
	g.drawImage(image, x, y, width, height, null);
    }
}
