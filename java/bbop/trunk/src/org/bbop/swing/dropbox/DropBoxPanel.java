package org.bbop.swing.dropbox;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.bbop.swing.SwingUtil;

import org.apache.log4j.*;

public class DropBoxPanel extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DropBoxPanel.class);

	protected Component glue;
	protected Component dropGhost;
	protected int orientation;

	public DropBoxPanel() {
		this(BoxLayout.Y_AXIS);
	}

	public DropBoxPanel(int orientation) {
		this.orientation = orientation;
		int ghostSize = 5;
		setLayout(new BoxLayout(this, orientation));
		dropGhost = new JPanel();
		if (orientation == BoxLayout.X_AXIS) {
			glue = Box.createHorizontalGlue();
			dropGhost.setMinimumSize(new Dimension(ghostSize, 0));
			dropGhost.setPreferredSize(new Dimension(ghostSize, 0));
			dropGhost
					.setMaximumSize(new Dimension(ghostSize, Integer.MAX_VALUE));
		} else {
			glue = Box.createVerticalGlue();
			dropGhost.setMinimumSize(new Dimension(0, ghostSize));
			dropGhost.setPreferredSize(new Dimension(0, ghostSize));
			dropGhost
					.setMaximumSize(new Dimension(Integer.MAX_VALUE, ghostSize));
		}
		dropGhost.setBackground(Color.green);
		add(glue);
		setDropTarget(new DropTarget(this, new DropTargetListener() {
			protected int dropPos;

			public void dragEnter(DropTargetDragEvent dtde) {
				// TODO Auto-generated method stub

			}

			public void dragExit(DropTargetEvent dte) {
				remove(dropGhost);
				SwingUtil.masterValidate(DropBoxPanel.this);
			}

			public void dragOver(DropTargetDragEvent dtde) {
				Point loc = dtde.getLocation();
				Component c = getComponentAt(loc.x, loc.y);

				Component contents = (Component) getContents(dtde);
				if (SwingUtilities
						.isDescendingFrom(DropBoxPanel.this, contents)) {
					dtde.rejectDrag();
					return;
				}

				if (c == null) {
					// do nothing
				} else if (c.equals(dropGhost)) {
					// do nothing
				} else if (c != DropBoxPanel.this) {
					Point reCalc = SwingUtilities.convertPoint(
							DropBoxPanel.this, loc, c);
					boolean isLeft = reCalc.x <= c.getWidth() / 2;
					boolean isTop = reCalc.y <= c.getHeight() / 2;
					int oldGhostIndex = SwingUtil.getIndex(DropBoxPanel.this,
							dropGhost);
					if (oldGhostIndex != -1)
						remove(dropGhost);
					int index = SwingUtil.getIndex(DropBoxPanel.this, c);
					if (index == -1) {
						dropPos = -1;
						dtde.rejectDrag();
						return;
					}
					boolean before;
					if (DropBoxPanel.this.orientation == BoxLayout.Y_AXIS) {
						before = isTop;
					} else
						before = isLeft;
					if (before)
						dropPos = index;
					else
						dropPos = index + 1;
					add(dropGhost, dropPos);
					SwingUtil.masterValidate(DropBoxPanel.this);
					dtde.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE);
				} else {
					remove(dropGhost);
				}
			}

			public void drop(DropTargetDropEvent dtde) {
				logger.info("dropPos = " + dropPos);
				if (dropPos == -1) {
					remove(dropGhost);
					SwingUtil.masterValidate(DropBoxPanel.this);
					dtde.rejectDrop();
					return;
				}
				Transferable t = dtde.getTransferable();
				Object o;
				try {
					o = t.getTransferData(DropBoxContentsTransferable.DROP_BOX_CONTENTS_FLAVOR);
					if (o != null && o instanceof DropBoxContents) {
						remove(dropGhost);
						DropBoxContents c = (DropBoxContents) o;
						int index = SwingUtil.getIndex(DropBoxPanel.this,
								(Component) c);
						if (index >= 0 && index < dropPos)
							dropPos--;
						if (index == dropPos) {
							dtde.rejectDrop();
						} else {
							Container p = ((Component) c).getParent();
							logger.info("removing " + c + " from " + p);
							SwingUtil.masterValidate((Component) c);
							add((Component) c, null, dropPos);
							dtde.acceptDrop(DnDConstants.ACTION_COPY_OR_MOVE);
						}
					}
					SwingUtil.masterValidate(DropBoxPanel.this);
				} catch (UnsupportedFlavorException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}

			protected DropBoxContents getContents(DropTargetDragEvent e) {
				Transferable t = e.getTransferable();
				Object o;
				try {
					o = t
							.getTransferData(DropBoxContentsTransferable.DROP_BOX_CONTENTS_FLAVOR);
					if (o != null && o instanceof DropBoxContents) {
						return (DropBoxContents) o;
					}
				} catch (Exception ex) {
				}
				return null;
			}

			public void dropActionChanged(DropTargetDragEvent dtde) {
			}

		}));
	}
	
	public List<DropBoxContents> getContents() {
		List<DropBoxContents> contents = new LinkedList<DropBoxContents>();
		for(Component c : getComponents()) {
			if (!c.equals(glue) && !c.equals(dropGhost)) {
				contents.add((DropBoxContents) c);
			}
		}
		return contents;
	}

	@Override
	protected void addImpl(Component comp, Object constraints, int index) {
		remove(glue);
		if (index > getComponentCount())
			index = getComponentCount();
		super.addImpl(comp, constraints, index);
		super.addImpl(glue, null, -1);
	}
}
