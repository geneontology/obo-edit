package org.bbop.swing;

import javax.swing.*;
import java.awt.*;

public class JDragPanel extends JPanel implements DragContainer {

    /**
	 * 
	 */
	private static final long serialVersionUID = -2403533422675994411L;
	protected Image dragImage = null;
    protected int dragX;
    protected int dragY;
    protected int width;
    protected int height;

    protected Rectangle scratchRect = new Rectangle();


    public JDragPanel() {
    }

    public void imageChanged(Image dragImage, int x, int y,
			     int width, int height) {
	int oldx = this.dragX;
	int oldy = this.dragY;
	int oldwidth = this.width;
	int oldheight = this.height;

	this.dragX = x;
	this.dragY = y;
	this.width = width;
	this.height = height;

	this.dragImage = dragImage;
	if (dragImage == null) {
	    repaint();
	    return;
	}

	scratchRect.setBounds(oldx, oldy, oldwidth, oldheight);
	SwingUtilities.computeUnion(x, y, width, height, scratchRect);
	repaint(scratchRect);
    }

    public void paint(Graphics g) {
	super.paint(g);
	paintDragImage(g);
    }

    public void paintDragImage(Graphics g) {
	if (dragImage != null)
	    g.drawImage(dragImage, dragX, dragY, null);
    }
}
