package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.image.ImageObserver;
import java.net.URL;

import javax.swing.JPanel;

import org.apache.log4j.Logger;

/**
 * This is an extract from the BackgroundImagePanel class from the bbop framework.
 */
public class BackgroundImagePanel extends JPanel implements ImageObserver {
	private static final long serialVersionUID = -5660377322773828337L;
	protected Image backgroundImage;
    protected volatile Image workingImage;
    protected Image tempImage;
    protected boolean scaleImage;
    protected boolean keepAspect = true;
    
	private static final Logger logger = Logger.getLogger(BackgroundImagePanel.class);

	public BackgroundImagePanel(URL imagePath, boolean scale) {
		super();
		super.setOpaque(false);
		setScaleImage(scale);

		setBackground(imagePath);
	}
	
	/**
	 * Enable/disable scaling of the background image.
	 * 
	 * @param scale
	 *            whether or not to scale the background image
	 */
	public void setScaleImage(boolean scale) {
		this.scaleImage = scale;
		if (scaleImage)
			resizeImage();
		else
			workingImage = backgroundImage;
	}

    public void setBackground(URL filename) {
    	// note: this really should be a call to createImage(), but it isn't
    	// supported in the JDK 1.1 . This should probably be replaced with
    	// some reflection code to call createImage() if it is available
    	Image image = Toolkit.getDefaultToolkit().getImage(filename);
            loadImage(image);
    	setBackground(image);
    }
    
    protected void loadImage(Image image) {
        MediaTracker tracker = new MediaTracker(this);
        synchronized(tracker) {
            tracker.addImage(image, 0);
            try {
                tracker.waitForID(0, 0);
            } catch (InterruptedException e) {
                logger.error("INTERRUPTED while loading Image");
            }
            tracker.removeImage(image, 0);
        }
	repaint();
    }


    /**
     * Sets the background image to the given Image.
     * @param image the new background image
     */ 
    public void setBackground(Image image) {
	this.backgroundImage = image;
	if (scaleImage)
	    resizeImage();
	else
	    workingImage = image;
    }
    
	protected void resizeImage() {
		int width = getSize().width;
		int height = getSize().height;

		if (backgroundImage != null && width > 0 && height > 0) {
			if (keepAspect) {
				double scaleFactor = (double) width
						/ (double) backgroundImage.getWidth(null);
				height = (int) (backgroundImage.getHeight(null) * scaleFactor);
				if (width > getWidth() || height > getHeight()) {
					height = getHeight();
					scaleFactor = (double) height
							/ (double) backgroundImage.getHeight(null);
					width = (int) (backgroundImage.getWidth(null) * scaleFactor);
				}
			}

			workingImage = backgroundImage.getScaledInstance(width, height,
					Image.SCALE_FAST);
			loadImage(workingImage);
		}
	}

	@Override
	public void setSize(int width, int height) {
		super.setSize(width, height);
		if (scaleImage)
			resizeImage();
	}

	@Override
	public void setSize(Dimension d) {
		super.setSize(d);
		if (scaleImage)
			resizeImage();
	}

	@Override
	public void setBounds(Rectangle r) {
		super.setBounds(r);
		if (scaleImage)
			resizeImage();
	}

	@Override
	public void setBounds(int x, int y, int width, int height) {
		super.setBounds(x, y, width, height);
		if (scaleImage)
			resizeImage();
	}

	@Override
	public void paint(Graphics g) {
		if (g != null) {
			if (backgroundImage != null) {
				int x = 0;
				int y = 0;
				if (workingImage.getWidth(null) < getWidth()) {
					x = (getWidth() - workingImage.getWidth(null)) / 2;
				}
				if (workingImage.getHeight(null) < getHeight()) {
					y = (getHeight() - workingImage.getHeight(null)) / 2;
				}
				g.drawImage(workingImage, x, y, null);
			}
			super.paint(g);
		}
	}
}