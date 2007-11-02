package org.oboedit.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Line2D;
import java.util.HashMap;

import javax.swing.Icon;
import javax.swing.tree.TreePath;

import org.bbop.swing.ZigZagStroke;
import org.obo.datamodel.Link;
import org.oboedit.gui.filter.LinkRenderSpec;

public class ArrowIcon implements Icon {

	protected final int[] triangleXBuffer = new int[3];

	protected final int[] triangleYBuffer = new int[3];

	protected final int triangleYSize = 4;

	protected final int triangleXSize = 4;

	protected final int triangleOffset = 2;

	protected final boolean arrowheadLeft = true;

	protected int width = 20;

	protected TreePath path;
	protected boolean isLeaf;
	protected Color color = Color.black;
	protected LineTypes lineType = LineTypes.SOLID_LINE;
	protected int lineWidth = 1;

	protected static HashMap strokeHash = new HashMap();

	// protected int right = 20;

	public int getIconHeight() {
		// TODO Auto-generated method stub
		return 10;
	}

	public int getIconWidth() {
		// TODO Auto-generated method stub
		return width;
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		try {
			if (path == null)
				return;
			Object value = path.getLastPathComponent();

			String hashVal = lineWidth + "-" + getLineType();
			Stroke stroke = (Stroke) strokeHash.get(hashVal);
			if (stroke == null) {
				float[] dash_pattern = null;
				if (getLineType() == LineTypes.DASHED_LINE
						|| getLineType() == LineTypes.DASHED_ZIGZAG_LINE) {
					dash_pattern = new float[2];
					dash_pattern[0] = width;
					dash_pattern[1] = width;
				}
				if (getLineType() == LineTypes.ZIGZAG_LINE
						|| getLineType() == LineTypes.DASHED_ZIGZAG_LINE) {
					stroke = new ZigZagStroke(new BasicStroke(getLineWidth(),
							BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1.0f,
							dash_pattern, 0), triangleYSize / 2, 4);
				} else {
					stroke = new BasicStroke(getLineWidth(),
							BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1.0f,
							dash_pattern, 0);
				}
				strokeHash.put(hashVal, stroke);
			}

			int xoffset = triangleOffset;

			boolean drawArrow = path.getPathCount() > 3;

			int modifiedLeft;

			if (drawArrow)
				modifiedLeft = x + xoffset + triangleXSize - 1;
			else
				modifiedLeft = x;

			g.setColor(getColor());
			Shape line = new Line2D.Float(modifiedLeft,
					y + getIconHeight() / 2, getIconWidth(), y
							+ getIconHeight() / 2);
			((Graphics2D) g).fill(stroke.createStrokedShape(line));
			// Stroke oldStroke = ((Graphics2D) g).getStroke();
			// ((Graphics2D) g).setStroke(stroke);
			// g.drawLine(modifiedLeft, y + getIconHeight() / 2, getIconWidth(),
			// y
			// + getIconHeight() / 2);
			//
			// ((Graphics2D) g).setStroke(oldStroke);
			if (drawArrow) {
				int centerY = y + getIconHeight() / 2;
				triangleYBuffer[0] = centerY - triangleYSize;
				triangleYBuffer[1] = centerY + triangleYSize;
				triangleYBuffer[2] = centerY;

				triangleXBuffer[0] = x + xoffset + triangleXSize;
				triangleXBuffer[1] = x + xoffset + triangleXSize;
				triangleXBuffer[2] = x + xoffset;

				g.fillPolygon(triangleXBuffer, triangleYBuffer, 3);
			}
		} catch (Throwable t) {
			t.printStackTrace();
		}

	}

	public TreePath getPath() {
		return path;
	}

	public void setPath(TreePath path) {
		this.path = path;
	}

	public boolean isLeaf() {
		return isLeaf;
	}

	public void setLeaf(boolean isLeaf) {
		this.isLeaf = isLeaf;
	}

	protected Color getColor() {
		return color;
	}

	protected void setColor(Color color) {
		this.color = color;
	}

	protected LineTypes getLineType() {
		return lineType;
	}

	protected void setLineType(LineTypes lineType) {
		this.lineType = lineType;
	}

	protected int getLineWidth() {
		return lineWidth;
	}

	protected void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}

}
