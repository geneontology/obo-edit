package org.bbop.swing;

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Event;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;

import javax.swing.CellRendererPane;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

public class ScalingComponent extends JComponent {
	protected JComponent scaleMe;
	protected boolean preserveAspect = true;

	protected CellRendererPane pane = new CellRendererPane();

	public ScalingComponent() {
		setLayout(null);
		enableEvents(AWTEvent.ACTION_EVENT_MASK
				| AWTEvent.ADJUSTMENT_EVENT_MASK
				| AWTEvent.COMPONENT_EVENT_MASK | AWTEvent.CONTAINER_EVENT_MASK
				| AWTEvent.FOCUS_EVENT_MASK
				| AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK
				| AWTEvent.INPUT_METHOD_EVENT_MASK | AWTEvent.ITEM_EVENT_MASK
				| AWTEvent.KEY_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK
				| AWTEvent.MOUSE_MOTION_EVENT_MASK
				| AWTEvent.MOUSE_WHEEL_EVENT_MASK | AWTEvent.PAINT_EVENT_MASK
				| AWTEvent.TEXT_EVENT_MASK | AWTEvent.WINDOW_EVENT_MASK
				| AWTEvent.WINDOW_FOCUS_EVENT_MASK
				| AWTEvent.WINDOW_STATE_EVENT_MASK);
	}

	public ScalingComponent(JComponent scaleMe) {
		this();
		add(pane);
		setScaledComponent(scaleMe);
	}

	public void setScaledComponent(JComponent scaleMe) {
		this.scaleMe = scaleMe;
		scaleMe.setSize(getPreferredSize());
		pane.add(scaleMe);
	}

	@Override
	protected void processMouseEvent(MouseEvent e) {
		int x = translateX(e.getX());
		int y = translateY(e.getY());

		scaleMe.setSize(scaleMe.getPreferredSize());
		scaleMe.validate();
		Component c = SwingUtilities.getDeepestComponentAt(scaleMe, x, y);
		if (c != null) {
			Point p = SwingUtilities.convertPoint(scaleMe, x, y, c);
			e = new MouseEvent(c, e.getID(), e.getWhen(), e.getModifiers(),
					p.x, p.x, e.getClickCount(), e.isPopupTrigger(), e
							.getButton());
			c.dispatchEvent(e);
		}
	}

	@Override
	protected void processMouseMotionEvent(MouseEvent e) {
		int x = translateX(e.getX());
		int y = translateY(e.getY());

		scaleMe.setSize(scaleMe.getPreferredSize());
		scaleMe.validate();
		Component c = SwingUtilities.getDeepestComponentAt(scaleMe, x, y);
		if (c != null) {
			Point p = SwingUtilities.convertPoint(scaleMe, x, y, c);
			e = new MouseEvent(c, e.getID(), e.getWhen(), e.getModifiers(),
					p.x, p.x, e.getClickCount(), e.isPopupTrigger(), e
							.getButton());
			c.dispatchEvent(e);
		}
	}

	@Override
	public boolean handleEvent(Event evt) {
		// TODO Auto-generated method stub
		return super.handleEvent(evt);
	}

	protected double internalGetXScale() {
		return (double) getWidth()
				/ (double) scaleMe.getPreferredSize().getWidth();
	}

	protected double internalGetYScale() {
		return (double) getHeight()
				/ (double) scaleMe.getPreferredSize().getHeight();
	}

	protected double getXScale() {
		if (preserveAspect)
			return Math.min(internalGetXScale(), internalGetYScale());
		else
			return internalGetXScale();
	}

	protected double getYScale() {
		if (preserveAspect)
			return Math.min(internalGetXScale(), internalGetYScale());
		else
			return internalGetYScale();
	}

	protected int translateX(double x) {
		double xoffset = (getWidth() - scaleMe.getPreferredSize().getWidth()
				* getXScale()) / 2;
		return (int) ((x - xoffset) / getXScale());
	}

	protected int translateY(double y) {
		double yoffset = (getHeight() - scaleMe.getPreferredSize().getHeight()
				* getYScale()) / 2;
		return (int) ((y - yoffset) / getYScale());
		// return (int) ((y / getYScale()) - yoffset);
	}

	@Override
	public void paint(Graphics g) {
		super.paint(g);
		Graphics2D g2 = (Graphics2D) g;
		// scaleMe.paint(g2);
		int width = getWidth();
		int height = getHeight();
		int xoffset = 0;
		int yoffset = 0;
		double xscale = internalGetXScale();
		double yscale = internalGetYScale();
		if (preserveAspect) {
			if (xscale < yscale) {
				height = (int) (scaleMe.getPreferredSize().getHeight() * xscale);
				yoffset = (getHeight() - height) / 2;
			} else {
				width = (int) (scaleMe.getPreferredSize().getWidth() * yscale);
				xoffset = (getWidth() - width) / 2;
			}
		}
		g2.translate(xoffset, yoffset);
		g2.scale(getXScale(), getYScale());
		pane.paintComponent(g, scaleMe, this, 0, 0, (int) scaleMe
				.getPreferredSize().getWidth(), (int) scaleMe
				.getPreferredSize().getHeight(), true);
	}
}
