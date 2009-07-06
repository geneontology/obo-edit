package org.bbop.swing.dropbox;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.geom.GeneralPath;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import org.bbop.swing.DragImageGenerator;
import org.bbop.swing.SwingUtil;

import org.apache.log4j.*;

public class DropBoxWrapper extends JPanel implements DropBoxContents {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DropBoxWrapper.class);

	protected Component contents;
	protected JComponent dragHandle;

	protected DragSourceListener dragListener = new DragSourceListener() {

		public void dragDropEnd(DragSourceDropEvent dsde) {
			if (!dsde.getDropSuccess()) {
				// startParent.add(DropBoxWrapper.this, null, startIndex);
			}
			// TODO Auto-generated method stub

		}

		public void dragEnter(DragSourceDragEvent dsde) {
			// TODO Auto-generated method stub

		}

		public void dragExit(DragSourceEvent dse) {
			// TODO Auto-generated method stub

		}

		public void dragOver(DragSourceDragEvent dsde) {
			// TODO Auto-generated method stub

		}

		public void dropActionChanged(DragSourceDragEvent dsde) {
			// TODO Auto-generated method stub

		}

	};

	public static class DragButton extends JButton implements DragImageGenerator {

		protected static final float Y_MARGIN = 2;
		protected static final float X_MARGIN = 2;

		public DragButton(String text) {
			super();
			setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			setMinimumSize(new Dimension(10, 0));
			setPreferredSize(getMinimumSize());
			// TODO Auto-generated constructor stub
		}

		public Icon getImage(DragSourceDragEvent event) {
			return new ImageIcon(SwingUtil.getImage(getParent()));
		}

		@Override
		protected void paintComponent(Graphics g) {
			float height = getHeight() - Y_MARGIN;
			float width = getWidth() - X_MARGIN;
			Graphics2D g2 = (Graphics2D) g;
			Object oldAI = g2.getRenderingHint(RenderingHints.KEY_ANTIALIASING);
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			float curveXDepth = Math.min(height, width) / 3;
			float curveYDepth = curveXDepth;
			GeneralPath p = new GeneralPath();
			p.moveTo(curveXDepth / 2 + X_MARGIN / 2, height + Y_MARGIN / 2);
			p.lineTo(width - X_MARGIN / 2, height + Y_MARGIN / 2);
			p.lineTo(width - X_MARGIN / 2, Y_MARGIN / 2);
			p.lineTo(width - X_MARGIN / 2 - curveXDepth, Y_MARGIN / 2);
			p.curveTo(curveXDepth / 2 + X_MARGIN / 2, Y_MARGIN / 2,
					X_MARGIN / 2, Y_MARGIN / 2 + curveYDepth / 2,
					X_MARGIN / 2, Y_MARGIN / 2 + curveYDepth);
			p.lineTo(X_MARGIN / 2, Y_MARGIN / 2 + height - curveYDepth);
			
			p.curveTo(X_MARGIN / 2, Y_MARGIN / 2 + height - curveYDepth / 2,		
					X_MARGIN / 2 + curveXDepth / 2, height + Y_MARGIN / 2,
					curveXDepth + X_MARGIN / 2, height + Y_MARGIN / 2);

			p.closePath();
			g2.setColor(getBackground());
			g2.fill(p);
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, oldAI);
		}

	}

	public DropBoxWrapper() {
		this(null);
	}

	protected Container startParent;
	protected DragGestureRecognizer currentRecognizer;
	protected int startIndex = -1;

	protected JComponent createDefaultDragComponent() {
		JComponent out = new DragButton("drag");
		return out;
	}
	
	public void setDragHandle(JComponent c) {
		DragSource ds = DragSource.getDefaultDragSource();
		if (dragHandle != null) {
			remove(dragHandle);
		}
		this.dragHandle = c;
		add(dragHandle, "West");
		currentRecognizer = ds.createDefaultDragGestureRecognizer(dragHandle,
				DnDConstants.ACTION_COPY_OR_MOVE, dragGestureListener);
		validate();
	}
	
	protected DragGestureListener dragGestureListener = new DragGestureListener() {

		public void dragGestureRecognized(DragGestureEvent dge) {
			try {
				Transferable t = new DropBoxContentsTransferable(
						DropBoxWrapper.this);
				/*
				 * startIndex = SwingUtil.getIndex(getParent(),
				 * DropBoxWrapper.this); startParent = getParent();
				 * logger.info("removing from "+startParent+"
				 * at index "+startIndex);
				 * startParent.remove(DropBoxWrapper.this);
				 * startParent.validate();
				 */
				dge.startDrag(DragSource.DefaultCopyNoDrop, t,
						dragListener);
			} catch (InvalidDnDOperationException e2) {
				e2.printStackTrace();
			}
		}

	};
	
	public JComponent getHandle() {
		return dragHandle;
	}

	public DropBoxWrapper(Component contents) {
		setLayout(new BorderLayout());
		if (contents != null)
			setContents(contents);
		dragHandle = createDefaultDragComponent();


		// DropBoxTransferHandler.installHandler(this, dragHandle);
	}

	public Component getContents() {
		return contents;
	}

	public void setContents(Component contents) {
		this.contents = contents;
		add(contents, "Center");
	}
}
