package org.bbop.swing;

import org.bbop.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class AutoScrollSpec {

    protected boolean doAutoscroll;
    protected JViewport viewport;
    protected Component component;
    protected Insets insets;
    
    protected int scrollInterval;
    protected int scrollDistance;
    protected VectorFilter scrollAllower;

    public AutoScrollSpec(Component component) {
	this(component, getViewportForComponent(component), null,
	     new Insets(10,10,10,10), 50, 10, true);
    }

    protected static JViewport getViewportForComponent(Component c) {
	return (JViewport) SwingUtilities.getAncestorOfClass(JViewport.class,
							     c);
    }

    public AutoScrollSpec(Component component,
			  JViewport viewport,
			  VectorFilter scrollAllower,
			  Insets insets,
			  int scrollInterval,
			  int scrollDistance,
			  boolean doAutoscroll) {
	this.component = component;
	this.doAutoscroll = doAutoscroll;
	this.scrollAllower = scrollAllower;
	this.scrollInterval = scrollInterval;
	this.scrollDistance = scrollDistance;
	this.viewport = viewport;
	this.insets = insets;
    }

    public boolean allowScroll(MouseEvent o) {
	return doAutoscroll &&
	    (scrollAllower == null ||
	     ((scrollAllower != null) &&
	     scrollAllower.satisfies(o)));
    }

    public VectorFilter getScrollAllower() {
	return scrollAllower;
    }

    public void setScrollAllower(VectorFilter scrollAllower) {
	this.scrollAllower = scrollAllower;
    }

    public void setViewport(JViewport viewport) {
	this.viewport = viewport;
    }

    public JViewport getViewport() {
	return viewport;
    }

    public Component getComponent() {
	return component;
    }

    public int getScrollDistance() {
	return scrollDistance;
    }

    public void setScrollDistance(int scrollDistance) {
	this.scrollDistance = scrollDistance;
    }

    public int getScrollInterval() {
	return scrollInterval;
    }

    public void setScrollInterval(int scrollInterval) {
	this.scrollInterval = scrollInterval;
    }

    public void setAutoscrollEnabled(boolean doAutoscroll) {
	this.doAutoscroll = doAutoscroll;
    }

    public boolean getAutoscrollEnabled() {
	return doAutoscroll;
    }

    public void setInsets(Insets insets) {
	this.insets = insets;
    }

    public Insets getInsets() {
	return insets;
    }
}
