package org.bbop.swing;

import javax.swing.*;
import java.awt.image.*;
import java.util.*;
import java.lang.ref.*;
import java.awt.*;

public class CachedPanel extends JPanel {

    /**
	 * 
	 */
	private static final long serialVersionUID = 8061084982652763299L;
	protected Dimension cacheDim;
    protected Hashtable rows;
    protected Rectangle clipBounds;
    protected boolean useSoftReferences = true;
    protected boolean useCache = true;

    private Color boxColor = Color.yellow;
    private ReferenceQueue queue = new ReferenceQueue();

    public CachedPanel() {
	super();
	cacheDim = new Dimension(100,100);
	rows = new Hashtable();
	clipBounds = new Rectangle();
    }

    public void setCacheDimensions(Dimension d) {
	cacheDim = new Dimension(d);
	// we could do something terribly clever and try to coalesce the
	// existing cache into properly sized chunks, but why bother?
	dumpCache();
    }

    public Dimension getCacheDimensions() {
	return cacheDim;
    }
    
    public void paint(Graphics g) {
	if (useCache) {
	    clipBounds = g.getClipBounds(clipBounds);
	    int sectorx = clipBounds.x / cacheDim.width;
	    int sectory = clipBounds.y / cacheDim.height;
	    int sectorWidth = 2 + (clipBounds.width / cacheDim.width);
	    int sectorHeight = 2 + (clipBounds.height / cacheDim.height);

	    for(int i=sectorx; i < sectorx+sectorWidth; i++) {
		for(int j=sectory; j < sectory+sectorHeight; j++) {
		    paintSector(g, i, j);
		}
	    }
	} else
	    doPaint(g);
    }

    protected void paintSector(Graphics g, int x, int y) {
	Integer intx = new Integer(x);
	Integer inty = new Integer(y);
	Object hashKey = getHashKey(intx, inty);
	int pixelx = x*cacheDim.width;
	int pixely = y*cacheDim.height;

	Image cached = fetchImage(hashKey);
	if (cached == null) {
	    cached = buildImage(cacheDim, pixelx, pixely);
	    storeImage(hashKey, cached);
	}

	g.drawImage(cached, pixelx, pixely, null);
	if (boxColor != null) {
	    g.setColor(boxColor);
	    g.drawRect(pixelx, pixely, cacheDim.width, cacheDim.height);
	}
    }

    protected void storeImage(Object key, Image image) {
	if (useSoftReferences) {
	    rows.put(key, new SoftReference(image));
	} else {
	    rows.put(key, image);
	}
    }

    protected Image fetchImage(Object key) {
	Image image = null;
	if (useSoftReferences) {
	    SoftReference ref = (SoftReference) rows.get(key);
	    if (ref != null)
		return (Image) ref.get();
	    else
		return null;
	} else {
	    return (Image) rows.get(key);
	}	
    }

    protected Image buildImage(Dimension cacheDim, int pixelx, int pixely) {
	Image cached = new BufferedImage(cacheDim.width,
					 cacheDim.height,
					 BufferedImage.TYPE_INT_ARGB);
	Graphics graphics = cached.getGraphics().create();
	graphics.translate(-pixelx, -pixely);
	graphics.setClip(pixelx, pixely, cacheDim.width, cacheDim.height);
	doPaint(graphics);
	return cached;
    }

    public void doPaint(Graphics g) {
	super.paint(g);
    }

    public void dumpCache(Rectangle dirtyRegion) {
	int sectorx = dirtyRegion.x / cacheDim.width;
	int sectory = dirtyRegion.y / cacheDim.height;
	int sectorWidth = 2 + (dirtyRegion.width / cacheDim.width);
	int sectorHeight = 2 + (dirtyRegion.height / cacheDim.height);

	for(int i=sectorx; i < sectorx+sectorWidth; i++) {
	    for(int j=sectory; j < sectory+sectorHeight; j++) {
		Object key = getHashKey(new Integer(i), new Integer(j));
		rows.remove(key);
	    }
	}
    }

    protected Object getHashKey(Integer x, Integer y) {
	return x+"-"+y;
    }

    public void dumpCache() {
	rows = new Hashtable();
    }

    public boolean getUseSoftReferences() {
	return useSoftReferences;
    }

    public void setUseSoftReferences(boolean use) {
	useSoftReferences = use;
	// actually, we could be clever and convert the existing cache
	dumpCache();
    }

    public boolean getUseCache() {
	return useCache;
    }

    public void setUseCache(boolean useCache) {
	this.useCache = useCache;
    }

    public static class Thing extends CachedPanel {
	/**
		 * 
		 */
		private static final long serialVersionUID = -2668631913940388574L;
	Rectangle clipRect = new Rectangle();
	public void doPaint(Graphics  g) {
	    clipRect = g.getClipBounds(clipRect);
	    g.setColor(Color.white);
	    g.fillRect(clipRect.x,clipRect.y,clipRect.width,clipRect.height);
	    for(int i=clipRect.x; i < clipRect.x+clipRect.width; i++) {
		for(int j=clipRect.y; j < clipRect.y+clipRect.height; j++) {
		    int count = i*getSize().width+j;
		    g.setColor(new Color(count));
		    g.drawLine(i,j,i,j);
		}
	    }
	}
    }

    public static void main(String [] args) {
	Thing thing = new Thing();
	thing.setPreferredSize(new Dimension(2000,1000));
	JScrollPane pane = new JScrollPane(thing);
	pane.setSize(200,200);
	pane.setPreferredSize(new Dimension(200,200));

	JDialog dialog = new JDialog();
	dialog.setTitle("Cached");
	dialog.getContentPane().add(pane);
	dialog.setSize(600,600);
	dialog.show();

    }
}













