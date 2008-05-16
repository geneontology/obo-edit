package org.oboedit.piccolo;

import java.util.Iterator;

import edu.umd.cs.piccolo.PNode;

import org.apache.log4j.*;

public class PiccoloBoxLayout implements PiccoloLayoutManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PiccoloBoxLayout.class);

	public static final int HORZ = 0;
	public static final int VERT = 1;
	public static final int LEFT_ALIGN = 0;
	public static final int RIGHT_ALIGN = 1;
	public static final int CENTER_ALIGN = 2;

	protected int orientation;
	protected double gap;

	public PiccoloBoxLayout(int orientation) {
		setOrientation(orientation);
	}

	public void setOrientation(int orientation) {
		this.orientation = orientation;
	}

	public void setGap(double gap) {
		this.gap = gap;
	}
	
	public static PNode createBox(int orientation) {
		PLayoutNode node = new PLayoutNode();
		node.setLayoutManager(new PiccoloBoxLayout(orientation));
		return node;
	}

	public void layoutChildren(PNode parent) {
		double currentSize = 0;
		double maxHeight = 0;
		double maxWidth = 0;
		Iterator it = parent.getChildrenIterator();
		while (it.hasNext()) {
			PNode node = (PNode) it.next();
			maxWidth = Math.max(node.getFullBoundsReference().getWidth(),
					maxWidth);
			maxHeight = Math.max(node.getFullBoundsReference().getHeight(),
					maxHeight);
		}

		it = parent.getChildrenIterator();
		boolean first = true;
		while (it.hasNext()) {
			PNode child = (PNode) it.next();
			if (first) {
				first = false;
			} else {
				currentSize += gap;
			}
			if (orientation == HORZ) {
				child.setOffset(currentSize, (maxHeight - child
						.getFullBoundsReference().getHeight()) / 2);
				currentSize += child.getFullBoundsReference().getWidth();
			} else {
				child.setOffset((maxWidth - child.getFullBoundsReference()
						.getWidth()) / 2, currentSize);
				currentSize += child.getFullBoundsReference().getHeight();
			}
		}
	}
}
