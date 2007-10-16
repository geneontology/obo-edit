package org.bbop.framework.dock.idw;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Icon;

import org.bbop.swing.RecolorableImageIcon;

import net.infonode.gui.icon.button.AbstractButtonIcon;

public class BitmapIcon extends AbstractButtonIcon {

	Map<Color, Icon> iconMap = new HashMap<Color, Icon>();
	
	protected int size;
	protected Image image;
	protected Color replaceme;

	public BitmapIcon(Image image) {
		super(Color.black);
		init(image, Color.black, 10);
	}

	protected void init(Image image, Color replaceme, int size) {
		this.size = size;
		this.replaceme = replaceme;
		this.image = image;	
	}

	public BitmapIcon(Image image, Color color) {
		super(color);
		init(image, Color.black, 10);
	}

	public BitmapIcon(Image image, Color color, int size) {
		super(color, size);
		init(image, Color.black, size);
	}

	protected void paintIcon(Component c, Graphics g, int x1, int y1, int x2,
			int y2) {
		Icon icon = iconMap.get(g.getColor());
		if (icon == null) {
			icon = new RecolorableImageIcon(image, replaceme, g.getColor());
			iconMap.put(g.getColor(), icon);
		}
		int xoffset = (x2 - x1 - icon.getIconWidth()) / 2;
		int yoffset = (y2 - y1 - icon.getIconHeight()) / 2;
		icon.paintIcon(c, g, x1+xoffset, y1+yoffset);
	}
}
