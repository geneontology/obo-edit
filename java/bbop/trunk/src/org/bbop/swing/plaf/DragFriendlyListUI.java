package org.bbop.swing.plaf;

import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.*;

import org.bbop.swing.*;

import org.apache.log4j.*;

public class DragFriendlyListUI extends javax.swing.plaf.basic.BasicListUI
	implements DragFriendlyUI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DragFriendlyListUI.class);

    protected boolean dragging = false;

    protected MouseInputListener createMouseInputListener() {
        return new MouseFriendlyInputHandler();
    }

    public void setIsDragging(boolean dragging) {
	this.dragging = dragging;
    }

    /**
     * Mouse input, and focus handling for JList.  An instance of this
     * class is added to the appropriate java.awt.Component lists
     * at installUI() time.  Note keyboard input is handled with JComponent
     * KeyboardActions, see installKeyboardActions().
     * <p>
     * <strong>Warning:</strong>
     * Serialized objects of this class will not be compatible with
     * future Swing releases.  The current serialization support is appropriate
     * for short term storage or RMI between applications running the same
     * version of Swing.  A future release of Swing will provide support for
     * long term persistence.
     *
     * @see #createMouseInputListener
     * @see #installKeyboardActions
     * @see #installUI
     */
    public class MouseFriendlyInputHandler implements MouseInputListener {
        public void mouseClicked(MouseEvent e) {}

        public void mouseEntered(MouseEvent e) {}

        public void mouseExited(MouseEvent e) {}

        public void mousePressed(MouseEvent e) {}

        public void mouseDragged(MouseEvent e) {}

        public void mouseMoved(MouseEvent e) {}

        public void mouseReleased(MouseEvent e) {
	    if (!SwingUtilities.isLeftMouseButton(e)) {
	        return;
	    }

	    if (!list.isEnabled()) {
		return;
	    }

	    /* Request focus before updating the list selection.  This implies
	     * that the current focus owner will see a focusLost() event
	     * before the lists selection is updated IF requestFocus() is
	     * synchronous (it is on Windows).  See bug 4122345
	     */
            if (!list.hasFocus()) {
                list.requestFocus();
            }

            int row = convertYToRow(e.getY());
            if (row != -1) {
                list.setValueIsAdjusting(true);
                int anchorIndex = list.getAnchorSelectionIndex();
                if (e.isControlDown()) {
                    if (list.isSelectedIndex(row)) {
                        list.removeSelectionInterval(row, row);
                    }
                    else {
                        list.addSelectionInterval(row, row);
                    }
                }
                else if (e.isShiftDown() && (anchorIndex != -1)) {
                    list.setSelectionInterval(anchorIndex, row);
                }
                else {
                    list.setSelectionInterval(row, row);
                }
		list.setValueIsAdjusting(false);
            }
        }
    }
}
