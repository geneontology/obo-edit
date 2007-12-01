package org.oboedit.gui.filter;

import java.awt.Color;

import org.oboedit.gui.ObjectSelector;

public class ConfiguredColor implements ColorProvider {
	protected Color color;
	protected boolean doBlend;

	public ConfiguredColor() {
	}

	public Color getColor(ObjectSelector fr, Object o) {
		return color;
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
}
