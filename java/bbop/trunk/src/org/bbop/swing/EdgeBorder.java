package org.bbop.swing;

import javax.swing.border.*;
import java.awt.*;

import org.apache.log4j.*;

public class EdgeBorder extends LineBorder {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EdgeBorder.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 121887443401755004L;
	protected boolean north;
    protected boolean south;
    protected boolean east;
    protected boolean west;

    public EdgeBorder(Color color, int thickness,
		      boolean north, boolean south,
		      boolean east, boolean west) {
	super(color, thickness);
	this.north = north;
	this.south = south;
	this.east = east;
	this.west = west;
    }
    /**
     * Returns the insets of the border.
     * @param c the component for which this border insets value applies
     */
    public Insets getBorderInsets(Component c)       {
        return getBorderInsets(c, new Insets(0,0,0,0));
    }

    /** 
     * Reinitialize the insets parameter with this Border's current Insets. 
     * @param c the component for which this border insets value applies
     * @param insets the object to be reinitialized
     */
    public Insets getBorderInsets(Component c, Insets insets) {
	if (north)
	    insets.top = thickness;
	if (south)
	    insets.bottom = thickness;
	if (west)
	    insets.left = thickness;
	if (east)
	    insets.right = thickness;
        return insets;
    }

    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        Color oldColor = g.getColor();
        int i;

	/// PENDING(klobad) How/should do we support Roundtangles?
        g.setColor(lineColor);
	if (north)
	    g.fillRect(x, y,
		       width, thickness);

	if (south)
	    g.fillRect(x, y+height-thickness,
		       width, thickness);

	if (east)
	    g.fillRect(x, y,
		       thickness, height);

	if (west)
	    g.fillRect(x+width-thickness, y,
		       thickness, height);
        g.setColor(oldColor);
    }
}
