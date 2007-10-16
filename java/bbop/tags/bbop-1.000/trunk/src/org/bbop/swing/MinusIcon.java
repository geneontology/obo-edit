package org.bbop.swing;

import java.awt.*;
import javax.swing.*;

public class MinusIcon implements Icon {

    protected Color color;
    protected float linewidth;
    protected int margin;
    protected BasicStroke stroke;
    protected int width;
    protected int height;

    public MinusIcon() {
	this(null, 2, 0, 10, 10);
    }

    public MinusIcon(float linewidth,
		     int width,
		     int height) {
	this(null, linewidth, 0, width, height);
    }

    public MinusIcon(int width, int height) {
	this(null, 2, 0, width, height);
    }

    public MinusIcon(Color color,
		     float linewidth,
		     int margin,
		     int width,
		     int height) {
	this.color  = color;
	this.linewidth = linewidth;
	this.margin = margin;
	this.height = height;
	this.width = width;
	stroke = new BasicStroke(linewidth);
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
	Color color = this.color;
	if (color == null)
	    color = c.getForeground();
	if (!c.isEnabled())
	    color = c.getBackground().darker();
	g.setColor(color);
	if (g instanceof Graphics2D)
	    ((Graphics2D) g).setStroke(stroke);
	int ycenter = height / 2;
	g.drawLine(x+margin, y+height/2, x+width-margin, y+height/2);
    }

    public int getIconWidth() {
	return width;
    }

    public int getIconHeight() {
	return height;
    }
}
