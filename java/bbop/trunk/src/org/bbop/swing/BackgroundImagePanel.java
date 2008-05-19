package org.bbop.swing;

import javax.swing.*;
import java.awt.image.*;
import java.awt.*;
import java.net.*;

/**
 * Implementation of JPanel that allows the background to be set to an image.
 * The BackgroundImagePanel can be set up to resize the background image to
 * fit the panel by calling {@link #setScaleImage(boolean)}. If no background
 * image is set, the panel is transparent.
 */
import org.apache.log4j.*;

public class BackgroundImagePanel extends JPanel implements ImageObserver {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BackgroundImagePanel.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = -7879156272563295319L;
	protected Image backgroundImage;
    protected volatile Image workingImage;
    protected Image tempImage;
    protected boolean scaleImage;
    protected boolean keepAspect = true;

    /**
     * Constructs a BackgroundImagePanel with no image and with scaling turned
     * off. Same as calling {@link #BackgroundImagePanel(boolean) BackgroundImagePanel(false)}.
     */
    public BackgroundImagePanel() {
	this(false);
    }

    /**
     * Constructs a BackgroundImagePanel with no image specified.
     * @param scale whether or not to scale the background image
     */    
    public BackgroundImagePanel(boolean scale) {
	super();
	super.setOpaque(false);
	setScaleImage(scale);
    }

    /**
     * Constructs a BackgroundImagePanel using the specified Image object
     * as the background. Scaling is disabled.
     * @param image the background image
     */    
    public BackgroundImagePanel(Image image) {
	this();
	setBackground(image);
    }


    /**
     * Constructs a BackgroundImagePanel using the image at the given path on
     * the local filesystem. Scaling is disabled.
     * @param imagePath path to the background image
     */    
    public BackgroundImagePanel(String imagePath) {
	this();
	setBackground(imagePath);
    }

    /**
     * Constructs a BackgroundImagePanel using the image at the given URL.
     * Scaling is disabled. See {@link #setBackground(URL)} for possible
     * performance issues with this method.
     * @param URL path to the background image
     */
    public BackgroundImagePanel(URL imagePath) {
	this();
	setBackground(imagePath);
    }    

    /**
     * Constructs a BackgroundImagePanel using the specified Image object
     * as the background.
     * @param image the background image
     * @param scale whether or not to scale the background image
     */
    public BackgroundImagePanel(Image image, boolean scale) {
	this(scale);
	setBackground(image);
    }

    /**
     * Constructs a BackgroundImagePanel using the image at the given path on
     * the local filesystem.
     * @param imagePath path to the background image
     * @param scale whether or not to scale the background image
     */    
    public BackgroundImagePanel(String imagePath, boolean scale) {
	this(scale);
	setBackground(imagePath);
    }

    /**
     * Constructs a BackgroundImagePanel using the image at the given URL.
     * See {@link #setBackground(URL)} for possible performance issues
     * with this method.
     * @param imagePath path to the background image
     * @param scale whether or not to scale the background image
     */
    public BackgroundImagePanel(URL imagePath, boolean scale) {
	this(scale);
	setBackground(imagePath);
    }

    public void setKeepAspect(boolean keepAspect) {
	this.keepAspect = keepAspect;
	if (scaleImage)
	    resizeImage();
    }

    /**
     * Enable/disable scaling of the background image.
     * @param scale whether or not to scale the background image
     */
    public void setScaleImage(boolean scale) {
	this.scaleImage = scale;
	if (scaleImage)
	    resizeImage();
	else
	    workingImage = backgroundImage;
    }

    /**
     * Sets the background image to the image at the given path on the local
     * filesystem. If the file cannot be found, the background image is not
     * set.
     * @param filename path to the new background image
     */
    public void setBackground(String filename) {
	// note: this really should be a call to createImage(), but it isn't
	// supported in the JDK 1.1 . This should probably be replaced with
	// some reflection code to call createImage() if it is available
	Image image = Toolkit.getDefaultToolkit().getImage(filename);
	setBackground(image);
    }

    /**
     * Sets the background image to the image at the given URL. If the file
     * cannot be found, the background image is not set. In order to guarantee
     * that the image is displayed, all the setBackground() methods stall
     * the current thread until the image is fully loaded. This can really
     * slow down GUI initialization if you are loading an image over a slow
     * network. If you'd prefer a more elegant solution to the image loading
     * problem than just blocking, you should load the image yourself and then
     * call {@link #setBackground(Image)}
     * @param filename URL of the new background image
     */
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
        int loadStatus;
        synchronized(tracker) {
            tracker.addImage(image, 0);
            try {
                tracker.waitForID(0, 0);
            } catch (InterruptedException e) {
                System.out.println("INTERRUPTED while loading Image");
            }
            loadStatus = tracker.statusID(0, false);
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
	//SwingUtil.blockUntilImagePrepared(backgroundImage);
	if (scaleImage)
	    resizeImage();
	else
	    workingImage = image;
    }

    public Image getImage() {
	return backgroundImage;
    }

    /**
     * Forces the background image cache to be recalculated. This is called
     * whenever the panel is resized.
     */
    protected void resizeImage() {
	int width = getSize().width;
	int height = getSize().height;

	if (backgroundImage != null && width > 0 && height > 0) {
	    if (keepAspect) {
		// try a width scale first...
		double scaleFactor = (double) width /
		    (double) backgroundImage.getWidth(null);
		height = (int) (backgroundImage.getHeight(null) *
				scaleFactor);
		// if it doesn't fit, scale on height
		if (width > getWidth() ||
		    height > getHeight()) {
		    height = getHeight();
		    scaleFactor = (double) height /
			(double) backgroundImage.getHeight(null);
		    width = (int) (backgroundImage.getWidth(null) *
				   scaleFactor);
		}
	    }

	    workingImage = backgroundImage.
		getScaledInstance(width, height, Image.SCALE_FAST);
            loadImage(workingImage);
	    //SwingUtil.blockUntilImagePrepared(workingImage);
	}
    }

    public void setSize(int width, int height) {
	super.setSize(width, height);
	if (scaleImage)
	    resizeImage();
    }

    public void setSize(Dimension d) {
	super.setSize(d);
	if (scaleImage)
	    resizeImage();
    }

    public void setBounds(Rectangle r) {
	super.setBounds(r);
	if (scaleImage)
	    resizeImage();
    }

    public void setBounds(int x, int y, int width, int height) {
	super.setBounds(x,y,width,height);
	if (scaleImage)
	    resizeImage();
    }

    public void paint(Graphics g) {
	if (g != null) {
	    if (backgroundImage != null) {
		int x = 0;
		int y = 0;
		if (workingImage.getWidth(null) <
		    getWidth()) {
		    x = (getWidth() - workingImage.getWidth(null)) / 2;
		}
		if (workingImage.getHeight(null) <
		    getHeight()) {
		    y = (getHeight() - workingImage.getHeight(null)) / 2;
		}
		g.drawImage(workingImage, x, y, null);
	    }
	    super.paint(g);
	}
    }
}
