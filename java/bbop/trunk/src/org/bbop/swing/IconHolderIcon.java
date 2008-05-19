package org.bbop.swing;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;

import org.apache.log4j.*;

public class IconHolderIcon implements Icon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IconHolderIcon.class);

	protected Icon icon;

	protected int defaultHeight;

	protected int defaultWidth;

	public IconHolderIcon(Icon icon) {
		this(icon, (icon == null ? 10 : icon.getIconWidth()),
				(icon == null ? 10 : icon.getIconHeight()));
	}

	public IconHolderIcon(Icon icon, int defaultWidth, int defaultHeight) {
		setIcon(icon);
		this.defaultHeight = defaultHeight;
		this.defaultWidth = defaultWidth;
	}
	
	public void setIcon(Icon icon) {
		this.icon = icon;
	}
	
	public Icon getIcon() {
		return icon;
	}

	public int getIconHeight() {
		if (icon == null)
			return defaultHeight;
		else
			return icon.getIconHeight();
	}

	public int getIconWidth() {
		if (icon == null)
			return defaultWidth;
		else
			return icon.getIconWidth();
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		if (icon != null) {
			icon.paintIcon(c, g, x, y);
		}
	}

}
