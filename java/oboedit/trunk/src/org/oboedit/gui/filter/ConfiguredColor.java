package org.oboedit.gui.filter;

import java.awt.Color;

public class ConfiguredColor {
	protected Color color;
	protected boolean doBlend;

	public ConfiguredColor() {
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public boolean isDoBlend() {
		return doBlend;
	}

	public void setDoBlend(boolean doBlend) {
		this.doBlend = doBlend;
	}

	public ConfiguredColor(Color color, boolean doBlend) {
		super();
		this.color = color;
		this.doBlend = doBlend;
	}

	@Override
	public String toString() {
		return color + (doBlend ? " (blend)" : "");
	}

	protected static Color getColor(double[] c) {
		return new Color((int) c[0], (int) c[1], (int) c[2]);
	}

	protected static void fillColor(double[] d, Color c) {
		d[0] = c.getRed();
		d[1] = c.getBlue();
		d[2] = c.getGreen();
	}

	protected static void mergeColor(double[] d, Color c) {
		d[0] = (d[0] + c.getRed()) / 2;
		d[1] = (d[0] + c.getGreen()) / 2;
		d[2] = (d[0] + c.getBlue()) / 2;
	}

	public static ConfiguredColor merge(ConfiguredColor... colors) {
		if (!colors[0].isDoBlend()) {
			return new ConfiguredColor(colors[0].getColor(), false);
		}
		double[] temp = new double[3];
		fillColor(temp, colors[0].getColor());
		for (int i = 1; i < colors.length; i++) {
			if (!colors[i].isDoBlend()) {
				return new ConfiguredColor(colors[i].getColor(), false);
			} else {
				mergeColor(temp, colors[i].getColor());
			}
		}
		return new ConfiguredColor(getColor(temp), true);
	}
}
