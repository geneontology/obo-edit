package org.bbop.swing;

import java.awt.Point;
import javax.swing.JViewport;

import org.apache.log4j.*;

public class FreezableViewport extends JViewport {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FreezableViewport.class);
    /**
	 * 
	 */
	private static final long serialVersionUID = 1642697341607090142L;
	protected boolean frozen = false;
    
    public void setFrozen(boolean frozen) {
	this.frozen = frozen;
    }
    
    public void setViewPosition(Point p) {
	if (!frozen)
	    super.setViewPosition(p);
    }
}
