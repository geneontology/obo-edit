package org.bbop.swing;

import java.awt.Component;
import java.awt.Point;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DragSourceMotionListener;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JComponent;

public class GhostImageController implements DragSourceListener,
		DragSourceMotionListener {
	
	protected GhostedDragImage ghost;
	protected static GhostImageController instance;
	protected static List<DragImageGenerator> generators =
		new LinkedList<DragImageGenerator>();
	protected DragSourceDragEvent lastEvent;

	public static GhostImageController getInstance() {
		if (instance == null)
			instance = new GhostImageController();
		return instance;
	}

	public static void enable() {
		DragSource.getDefaultDragSource().addDragSourceListener(getInstance());
		DragSource.getDefaultDragSource().addDragSourceMotionListener(
				getInstance());
	}

	public static void disable() {
		DragSource.getDefaultDragSource().removeDragSourceListener(
				getInstance());
		DragSource.getDefaultDragSource().removeDragSourceMotionListener(
				getInstance());
	}

	public static void addImageGenerator(DragImageGenerator generator) {
		generators.add(generator);
	}
	
	public static void removeImageGenerator(DragImageGenerator generator) {
		generators.remove(generator);
	}

	protected void check(DragSourceDragEvent e) {
		lastEvent = e;
		Component src = e.getDragSourceContext().getComponent();
		if (ghost == null && src instanceof JComponent && src.isVisible()) {
			JComponent c = (JComponent) src;
			Point screen = e.getLocation();
			Point origin = c.getLocationOnScreen();
/*
			Point imageOffset = new Point(origin.x - screen.x, origin.y
					- screen.y);
					*/
			Point imageOffset = new Point(0,0);
			Icon icon = null;
			boolean opaque = false;
			if (c instanceof DragImageGenerator) {
				icon = ((DragImageGenerator) c).getImage(e);
			}
			if (icon == null) {
				for(DragImageGenerator gen : generators) {
					icon = gen.getImage(e);
					if (icon != null)
						break;
				}
			}
			/*
			 * if (src instanceof JTree) { JTree tree = (JTree)c; if
			 * (tree.getDragEnabled()) { icon = new TreeSelectionIcon(tree); } }
			 * else if (src instanceof JTable) { JTable table = (JTable)c; if
			 * (table.getDragEnabled()) { icon = new TableSelectionIcon(table); } }
			 * else if (src instanceof JList) { JList list = (JList)c; if
			 * (list.getDragEnabled()) { icon = new ListSelectionIcon(list); } }
			 * else if (src instanceof JTextComponent) { JTextComponent text =
			 * (JTextComponent)c; if (text.getDragEnabled()) { icon = new
			 * TextSelectionIcon(text); } } else if (src instanceof
			 * JColorChooser) { JColorChooser chooser = (JColorChooser)c; if
			 * (chooser.getDragEnabled()) { icon = new
			 * ColorSelectionIcon(chooser); imageOffset.x =
			 * -icon.getIconWidth()/2; imageOffset.y = -icon.getIconHeight()/2;
			 * opaque = true; } }
			 */
			if (icon != null) {
				ghost = new GhostedDragImage(c, e.getLocation(), icon,
						imageOffset);
				if (opaque)
					ghost.setAlpha(1f);
			}
		}
	}
	
	public void forceImageUpdate() {
		if (lastEvent != null && ghost != null) {
			ghost.dispose();
			ghost = null;
			check(lastEvent);
		}
	}

	public void dragEnter(DragSourceDragEvent e) {
		check(e);
		if (ghost != null) {
			ghost.move(e.getLocation());
		}
	}

	public void dragOver(DragSourceDragEvent e) {
		check(e);
		if (ghost != null)
			ghost.move(e.getLocation());
	}

	public void dropActionChanged(DragSourceDragEvent e) {
		check(e);
		if (ghost != null)
			ghost.move(e.getLocation());
	}

	public void dragDropEnd(DragSourceDropEvent e) {
		if (ghost != null) {
			if (e.getDropSuccess()) {
				ghost.dispose();
			} else {
				ghost.returnToOrigin();
			}
			ghost = null;
		}
	}

	public void dragExit(DragSourceEvent e) {
	}

	public void dragMouseMoved(DragSourceDragEvent e) {
		check(e);
		if (ghost != null)
			ghost.move(e.getLocation());
	}
}
