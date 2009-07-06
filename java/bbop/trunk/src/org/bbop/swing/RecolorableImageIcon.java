package org.bbop.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import javax.swing.Icon;
import javax.swing.ImageIcon;

/**
 * Allows some color in an ImageIcon to be replaced with another color via the
 * setColor() method. This is most useful if you'd like to use a black and white bitmap
 * image as a template, and then apply new colors to it as you wish.
 */

import org.apache.log4j.*;

public class RecolorableImageIcon implements Icon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RecolorableImageIcon.class);

	protected Color color;
	protected Color replaceColor;
	protected Image originalImage;
	protected ImageIcon imageIcon;
	
	public RecolorableImageIcon(ImageIcon icon, Color replaceColor, Color withColor) {
		this(icon.getImage(), replaceColor, withColor);
	}

	public RecolorableImageIcon(Image image, Color replaceColor, Color withColor) {
		this.originalImage = image;
		this.replaceColor = replaceColor;
		setColor(withColor);
	}
	
	
	public void setColor(Color color) {
		MapColorsFilter filter = new MapColorsFilter(replaceColor.getRGB(), color.getRGB());
		new ImageIcon(originalImage);
		Image filtered = filter.filter(ImageUtils.convertImageToARGB(originalImage), null);
		imageIcon = new ImageIcon(filtered);
		this.color = color;
	}

	public int getIconHeight() {
		return imageIcon.getIconHeight();
	}

	public int getIconWidth() {
		return imageIcon.getIconWidth();
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		imageIcon.paintIcon(c, g, x, y);
	}
}
