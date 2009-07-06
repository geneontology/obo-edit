package org.bbop.swing.plaf;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.plaf.basic.*;

import org.bbop.swing.*;

import org.apache.log4j.*;

public class DragFriendlyTreeUI extends BasicTreeUI implements DragFriendlyUI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DragFriendlyTreeUI.class);

	public static class TreeIcon implements Icon {

		public final static int PLUS = 1;
		public final static int MINUS = 2;

		private final static int width = 8;
		private final static int height = 8;

		private int type;
		private Color foreground;
		private Color background;

		public TreeIcon(int type) {
			this(type, Color.black);
		}

		public TreeIcon(int type, Color foreground) {
			this.type = type;
			this.background = Color.white;
			this.foreground = foreground;
		}

		public int getIconHeight() {
			return width;
		}

		public int getIconWidth() {
			return height;
		}

		public void paintIcon(Component c, Graphics g, int x, int y) {
			g.setColor(Color.white);
			g.fillRect(x, y, width, height);
			g.setColor(foreground);
			g.drawRect(x, y, width, height);
			g.drawLine(x + 2, y + (height / 2), x + 6, y + (height / 2));
			if (type == PLUS)
				g.drawLine(x + (width / 2), y + 2, x + (width / 2), y + 6);
		}
	}

	protected boolean disableEvents = false;
	protected boolean dragging = false;
	protected static int idgen = 0;

	protected int id = ++idgen;

	@Override
	public String toString() {
		return "DragFriendlyTreeUI " + id;
	}

	public DragFriendlyTreeUI() {
		super();
		TreeIcon minusIcon = new TreeIcon(TreeIcon.MINUS);
		TreeIcon plusIcon = new TreeIcon(TreeIcon.PLUS);

		UIManager.put("Tree.expandedIcon", minusIcon);
		UIManager.put("Tree.collapsedIcon", plusIcon);

		setExpandedIcon(new TreeIcon(TreeIcon.MINUS));
		setCollapsedIcon(new TreeIcon(TreeIcon.PLUS));
		setRightChildIndent(minusIcon.getIconWidth()+4);

	}

	@Override
	protected void installDefaults() {
		super.installDefaults();
	}

	@Override
	public void setRightChildIndent(int newAmount) {
		// TODO Auto-generated method stub
		super.setRightChildIndent(newAmount);
	}

	@Override
	protected void updateSize() {
		validCachedPreferredSize = false;
		if (tree != null)
			tree.treeDidChange();
	}

	@Override
	protected int getRowX(int row, int depth) {
		// TODO Auto-generated method stub
		return super.getRowX(row, depth);
	}

	public void setIsDragging(boolean dragging) {
		this.dragging = dragging;
	}

	@Override
	protected MouseListener createMouseListener() {
		return new DragFriendlyMouseHandler();
	}

	/**
	 * Delegates to the original mouse handler, but uses mouseReleased events
	 */
	public class DragFriendlyMouseHandler extends MouseAdapter {

		@Override
		public void mouseReleased(MouseEvent e) {
			if (dragging || e.isConsumed()) {
				return;
			}

			if (tree != null && tree.isEnabled()) {
				tree.requestFocus();
				TreePath path = getClosestPathForLocation(tree, e.getX(), e
						.getY());

				if (path != null) {
					Rectangle bounds = getPathBounds(tree, path);

					if (e.getY() > (bounds.y + bounds.height)) {
						return;
					}

					// Preferably checkForClickInExpandControl could take
					// the Event to do this it self!
					if (SwingUtilities.isLeftMouseButton(e))
						checkForClickInExpandControl(path, e.getX(), e.getY());

					int x = e.getX();

					// Perhaps they clicked the cell itself. If so,
					// select it.

					if (x > bounds.x) {
						if (x <= (bounds.x + bounds.width)
								&& !startEditing(path, e)) {
							selectPathForEvent(path, e);
						}
					}
				}
			}
			// open the folder icon) until mouse up.
		}
	}
}
