package org.bbop.swing.plaf;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.plaf.basic.*;

import org.bbop.swing.*;

public class DragFriendlyTreeUI extends BasicTreeUI implements DragFriendlyUI {

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
	    g.fillRect(x,y,width,height);
	    g.setColor(foreground);
	    g.drawRect(x,y,width,height);
	    g.drawLine(x+2, y+(height/2), x+6, y+(height/2));
	    if (type == PLUS)
		g.drawLine(x+(width/2), y+2, x+(width/2), y+6);
	}
    }

    protected boolean disableEvents = false;
    protected boolean dragging = false;
    protected static int idgen = 0;

    protected int id = ++idgen;

    public String toString() {
	return "DragFriendlyTreeUI "+id;
    }
    
    public DragFriendlyTreeUI() {
	super();
	TreeIcon minusIcon = new TreeIcon(TreeIcon.MINUS);
	TreeIcon plusIcon = new TreeIcon(TreeIcon.PLUS);

	UIManager.put("Tree.expandedIcon", minusIcon);
	UIManager.put("Tree.collapsedIcon", plusIcon);

	setExpandedIcon(new TreeIcon(TreeIcon.MINUS));
	setCollapsedIcon(new TreeIcon(TreeIcon.PLUS));

    }

    public void setIsDragging(boolean dragging) {
	this.dragging = dragging;
    }

    protected MouseListener createMouseListener() {
        return new DragFriendlyMouseHandler();
    }

    /**
     * Delegates to the original mouse handler, but uses mouseReleased
     * events 
     */
    public class DragFriendlyMouseHandler extends MouseAdapter  {

	public void mouseReleased(MouseEvent e) {
	    if (dragging || e.isConsumed()) {
		return;
	    }
		
	    if(tree != null && tree.isEnabled()) {
		tree.requestFocus();
		TreePath     path = getClosestPathForLocation(tree, e.getX(),
							      e.getY());

		if(path != null) {
		    Rectangle       bounds = getPathBounds(tree, path);

		    if(e.getY() > (bounds.y + bounds.height)) {
			return;
		    }

		    // Preferably checkForClickInExpandControl could take
		    // the Event to do this it self!
		    if(SwingUtilities.isLeftMouseButton(e))
			checkForClickInExpandControl(path, e.getX(), e.getY());

		    int x = e.getX();

		    // Perhaps they clicked the cell itself. If so,
		    // select it.

		    if (x > bounds.x) {
			if (x <= (bounds.x + bounds.width) && 
			    !startEditing(path, e)) {
			    selectPathForEvent(path, e);
			}
		    }
		}
	    }
	    // open the folder icon) until mouse up.
	}

    }

   protected void paintHorizontalPartOfLeg(Graphics g, Rectangle clipBounds,
					    Insets insets, Rectangle bounds,
					    TreePath path, int row,
					    boolean isExpanded,
					    boolean hasBeenExpanded, boolean
					    isLeaf) {
        // Don't paint the legs for the root'ish node if the
        int depth = path.getPathCount() - 1;
	if((depth == 0 || (depth == 1 && !isRootVisible())) &&
	   !getShowsRootHandles()) {
	    return;
        }

	int clipLeft = clipBounds.x;
	int clipRight = clipBounds.x + (clipBounds.width - 1);
	int clipTop = clipBounds.y;
	int clipBottom = clipBounds.y + (clipBounds.height - 1);
	int lineY = bounds.y + bounds.height / 2;

	int leftX = bounds.x - getRightChildIndent();
	int nodeX = bounds.x - getHorizontalLegBuffer();
	
	if(lineY >= clipTop && lineY <= clipBottom && nodeX >= clipLeft &&
	   leftX <= clipRight ) {
	    leftX = Math.max(Math.max(insets.left, leftX), clipLeft);
	    nodeX = Math.min(Math.max(insets.left, nodeX), clipRight);
	    
	    if (leftX != nodeX) {
		g.setColor(getHashColor());
		paintHorizontalLine(g, tree, lineY, leftX, nodeX, isLeaf,
				    path);
	    }
	}
    }

    /**
     * Paints a horizontal line.
     */
    int [] triangleXBuffer = new int[3];
    int [] triangleYBuffer = new int[3];
    protected int triangleYSize = 4;
    protected int triangleXSize = 4;
    protected int triangleOffset = 2;
    protected int controlWidth = 8;

    public void setTriangleYSize(int triangleYSize) {
	this.triangleYSize = triangleYSize;
    }

    public void setTriangleXSize(int triangleXSize) {
	this.triangleXSize = triangleXSize;
    }

    public void setTriangleOffset(int triangleOffset) {
	this.triangleOffset = triangleOffset;
    }

    protected void paintHorizontalLine(Graphics g, JComponent c, int y,
				       int left, int right, boolean isLeaf,
				       TreePath path) {
	g.drawLine(left, y, right, y);
	if (drawArrowhead && path.getPathCount() > 2) {
	    triangleYBuffer[0] = y - triangleYSize;
	    triangleYBuffer[1] = y + triangleYSize;
	    triangleYBuffer[2] = y;
	    
	    int xoffset = triangleOffset;
	    if (!isLeaf) {
		xoffset += 2 + controlWidth / 2;
	    }
	    if (arrowheadLeft) {
		triangleXBuffer[0] = left + xoffset + triangleXSize;
		triangleXBuffer[1] = left + xoffset + triangleXSize;
		triangleXBuffer[2] = left + xoffset;
	    } else {
		triangleXBuffer[0] = right - xoffset - triangleXSize;
		triangleXBuffer[1] = right - xoffset - triangleXSize;
		triangleXBuffer[2] = right - xoffset;
	    }
	    g.fillPolygon(triangleXBuffer, triangleYBuffer, 3);
	}
    }

    public void setDrawArrowhead(boolean drawArrowhead) {
	this.drawArrowhead = drawArrowhead;
    }

    protected boolean drawArrowhead = true;
    protected boolean arrowheadLeft = true;
}

