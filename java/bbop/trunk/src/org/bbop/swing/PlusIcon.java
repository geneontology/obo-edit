package org.bbop.swing;

import java.awt.*;

import org.apache.log4j.*;

public class PlusIcon extends MinusIcon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PlusIcon.class);

    public PlusIcon() {
	this(null, 2, 0, 10, 10);
    }

    public PlusIcon(float linewidth,
		     int width,
		     int height) {
	this(null, linewidth, 0, width, height);
    }

    public PlusIcon(int width, int height) {
	this(null, 2, 0, width, height);
    }

    public PlusIcon(Color color,
		    float linewidth,
		    int margin,
		    int width,
		    int height) {
	super(color, linewidth, margin, width, height);
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
	super.paintIcon(c, g, x, y);
	int xcenter = width / 2;
	g.drawLine(x+xcenter, y+margin, x+xcenter, y+height-margin);
    }
}
