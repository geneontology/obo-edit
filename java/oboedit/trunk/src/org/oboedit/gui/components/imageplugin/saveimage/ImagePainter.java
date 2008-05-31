/**
 * 
 */
package org.oboedit.gui.components.imageplugin.saveimage;

import java.awt.Graphics2D;

public interface ImagePainter {
	public void paint(Graphics2D g);
	public int getWidth();
	public int getHeight();
}