package org.bbop.swing;

import javax.swing.Icon;
import java.awt.*;
import java.util.Iterator;
import java.util.Vector;

import org.apache.log4j.*;

public class MultiIcon implements Icon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiIcon.class);

	protected Vector icons = new Vector();
	protected int iconSpacing = 5;
	static int i;

	public void paintIcon(Component c, Graphics g, int x, int y) {
//		logger.debug("\n # of calls to paintIcon i: " + i);
//		logger.debug(">> MultiIcon.paintIcon(Component c, Graphics g, int x, int y) ");
		int height = getIconHeight();
//		logger.debug("height: " + height);
		Iterator it = icons.iterator();
		while(it.hasNext()){
			Icon icon = (Icon) it.next();
//			logger.debug("-- icon: " +  icon);
//			logger.debug("paintIcon, g, x, y + (height - icon.getIconHeight()) / 2)");
//			logger.debug("x: " +x);
//			logger.debug("y: " + (y + (height - icon.getIconHeight()) / 2));
			icon.paintIcon(c, g, x, y + (height - icon.getIconHeight()) / 2);
			x += icon.getIconWidth() + iconSpacing;
//			logger.debug("next x: " + x);
		}
		i++;
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
		Iterator it = icons.iterator();
		while (it.hasNext()){
			Icon icon = (Icon) it.next();
//			logger.debug("icon.getIconHeight(): " + icon.getIconHeight());
//			logger.debug("maxHeight: " + maxHeight);
			if (icon.getIconHeight() > maxHeight){
				maxHeight = icon.getIconHeight();
//				logger.debug("MultiIcon.getIconHeight -- maxHeight: " + maxHeight + " & i: " + i);
			}
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
