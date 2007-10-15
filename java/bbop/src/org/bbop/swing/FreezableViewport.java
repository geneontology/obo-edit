package org.bbop.swing;

import java.awt.Point;
import javax.swing.JViewport;

public class FreezableViewport extends JViewport {
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
