package org.bbop.swing;

import javax.swing.JScrollPane;
import javax.swing.JViewport;
import java.awt.Component;

import org.apache.log4j.*;

public class FreezableScrollPane extends JScrollPane {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FreezableScrollPane.class);
    /**
	 * 
	 */
	private static final long serialVersionUID = -1225668964102133062L;
	protected boolean frozen = false;

    public FreezableScrollPane() {
	super();
    }

    public FreezableScrollPane(Component view) {
	super(view);
    }

    public FreezableScrollPane(Component view, int vsbPolicy, int hsbPolicy) {
	super(view, vsbPolicy, hsbPolicy);
    }

    public FreezableScrollPane(int vsbPolicy, int hsbPolicy) {
	super(vsbPolicy, hsbPolicy);
    }

    @Override
	protected JViewport createViewport() {
	return new FreezableViewport();
    }

    public void setFrozen(boolean frozen) {
	this.frozen = frozen;
	getVerticalScrollBar().setEnabled(!frozen);
	getHorizontalScrollBar().setEnabled(!frozen);
	if (getViewport() instanceof FreezableViewport)
	    ((FreezableViewport) getViewport()).setFrozen(frozen);
    }

    @Override
	public void setViewport(JViewport viewport) {
	super.setViewport(viewport);
	if (viewport instanceof FreezableViewport)
	    ((FreezableViewport) viewport).setFrozen(frozen);
    }

    public boolean getFrozen() {
	return frozen;
    }
}
