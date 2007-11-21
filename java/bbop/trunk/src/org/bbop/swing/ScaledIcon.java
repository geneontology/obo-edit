package org.bbop.swing;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Rectangle2D;

import javax.swing.Icon;

public class ScaledIcon implements Icon {

	protected int width = -1;
	protected int height = -1;
	protected double widthScaleFactor = 1;
	protected double heightScaleFactor = 1;
	protected Icon icon;
	protected boolean antialias;

	public ScaledIcon(Icon icon, int width, int height) {
		this.icon = icon;
		setDimension(width, height);
	}

	public ScaledIcon(Icon icon, int dim) {
		this.icon = icon;
		setDimension(dim);
	}

	public ScaledIcon(Icon icon) {
		setIcon(icon);
	}

	public void setAntialias(boolean antialias) {
		this.antialias = antialias;
	}

	public void setIcon(Icon icon) {
		this.icon = icon;
		if (icon == null)
			setDimension(0, 0);
		else
			this.setDimension(icon.getIconWidth(), icon.getIconWidth());
	}

	public void setDimension(int dimension) {
		int width = -1;
		int height = -1;

		if (icon != null) {
			if (icon.getIconWidth() < icon.getIconHeight())
				height = dimension;
			else
				width = dimension;
		}
		setDimension(width, height);
	}

	public void setDimension(int width, int height) {
		if (icon == null) {
			widthScaleFactor = 0;
			heightScaleFactor = 0;
			this.width = 0;
			this.height = 0;
			return;
		}
		widthScaleFactor = 1;
		heightScaleFactor = 1;
		if (height > 0)
			heightScaleFactor = (double) height / icon.getIconHeight();
		if (width > 0) {
			widthScaleFactor = (double) width / icon.getIconWidth();
		}
		if (width == -1) {
			widthScaleFactor = heightScaleFactor;
		}
		if (height == -1)
			heightScaleFactor = widthScaleFactor;
		width = (int) (icon.getIconWidth() * widthScaleFactor);
		height = (int) (icon.getIconHeight() * heightScaleFactor);
		this.width = width;
		this.height = height;
	}

	public int getIconHeight() {
		return height;
	}

	public int getIconWidth() {
		return width;
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		if (icon == null)
			return;
		if (widthScaleFactor == 1 && heightScaleFactor == 1) {
			icon.paintIcon(c, g, x, y);
			return;
		}
		Graphics2D g2 = (Graphics2D) g;
		if (antialias)
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
		g2.translate(x, y);
		g2.scale(widthScaleFactor, heightScaleFactor);
		icon.paintIcon(c, g, 0, 0);
		g2.scale(1 / widthScaleFactor, 1 / heightScaleFactor);
		g2.translate(-x, -y);
	}

}
