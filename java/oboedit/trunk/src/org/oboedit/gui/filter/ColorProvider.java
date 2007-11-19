package org.oboedit.gui.filter;

import java.awt.Color;

import org.oboedit.gui.ObjectSelector;

public interface ColorProvider {

	public Color getColor(ObjectSelector selector, Object o);
	public boolean isDoBlend();
}
