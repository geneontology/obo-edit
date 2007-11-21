package org.bbop.swing;

import javax.swing.Icon;
import java.awt.*;
import java.util.Vector;

public class MultiIcon implements Icon {

	protected Vector icons = new Vector();
	protected int iconSpacing = 5;

	public void paintIcon(Component c, Graphics g, int x, int y) {
		int height = getIconHeight();
		for (int i = 0; i < icons.size(); i++) {
			Icon icon = (Icon) icons.get(i);
			icon.paintIcon(c, g, x, y + (height - icon.getIconHeight()) / 2);
			x += icon.getIconWidth() + iconSpacing;
		}
	}

	public int getIconWidth() {
		int totalWidth = 0;
		for (int i = 0; i < icons.size(); i++) {
			Icon icon = (Icon) icons.get(i);
			if (i > 0)
				totalWidth += iconSpacing;
			totalWidth += icon.getIconWidth();
		}
		return totalWidth;
	}

	public int getIconHeight() {
		int maxHeight = 0;
		for (int i = 0; i < icons.size(); i++) {
			Icon icon = (Icon) icons.get(i);
			if (icon.getIconHeight() > maxHeight)
				maxHeight = icon.getIconHeight();
		}
		return maxHeight;
	}

	public void addIcon(Icon icon) {
		icons.add(icon);
	}

	public void removeIcon(Icon icon) {
		icons.remove(icon);
	}

	public void clearIcons() {
		icons.clear();
	}
}
