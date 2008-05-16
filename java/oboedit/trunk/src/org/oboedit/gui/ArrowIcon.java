package org.oboedit.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
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

import org.apache.log4j.*;

public class ArrowIcon implements Icon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ArrowIcon.class);

	protected final int[] triangleXBuffer = new int[3];

	protected final int[] triangleYBuffer = new int[3];

	protected final int triangleYSize = 4;

	protected final int triangleXSize = 4;

	protected final int triangleOffset = 2;

	protected final boolean arrowheadLeft = true;

	protected int width = 20;
	protected int no_arrow_width = 1;

	protected TreePath path;
	protected boolean isLeaf;
	protected Color color = Color.black;
	protected LineType lineType = LineType.SOLID_LINE;
	protected int lineWidth = 1;

	protected static HashMap strokeHash = new HashMap();

	// protected int right = 20;

	public int getIconHeight() {
		// TODO Auto-generated method stub
		return 10;
	}

	public int getIconWidth() {
		return width;
	}
	
	protected boolean shouldDrawArrow() {
		return path.getPathCount() > 3;
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		try {
			((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			if (path == null)
				return;
			Object value = path.getLastPathComponent();

			String hashVal = lineWidth + "-" + getLineType();
			Stroke stroke = null; // (Stroke) strokeHash.get(hashVal);
			if (stroke == null) {
				float[] dash_pattern = null;
				if (getLineType() == LineType.DASHED_LINE) {
					dash_pattern = new float[2];
					dash_pattern[0] = getLineWidth();
					dash_pattern[1] = getLineWidth()*2;
				}
				if (getLineType() == LineType.ZIGZAG_LINE) {
					stroke = new ZigZagStroke(new BasicStroke(getLineWidth(),
							BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1.0f,
							dash_pattern, 0), triangleYSize / 2, 4);
				} else {
					stroke = new BasicStroke(getLineWidth(),
							BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f,
							dash_pattern, 0);
				}
				strokeHash.put(hashVal, stroke);
			}

			int xoffset = triangleOffset;

			int modifiedLeft;

			if (shouldDrawArrow())
				modifiedLeft = x + xoffset + triangleXSize - 1;
			else
				modifiedLeft = x;

			g.setColor(getColor());
			Shape line = new Line2D.Float(modifiedLeft,
					y + getIconHeight() / 2, getIconWidth(), y
							+ getIconHeight() / 2);
			if (stroke instanceof BasicStroke) {
				((Graphics2D) g).setStroke(stroke);
				((Graphics2D) g).draw(line);
			} else {
				((Graphics2D) g).fill(stroke.createStrokedShape(line));
			}
			((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);

			// Stroke oldStroke = ((Graphics2D) g).getStroke();
			// ((Graphics2D) g).setStroke(stroke);
			// g.drawLine(modifiedLeft, y + getIconHeight() / 2, getIconWidth(),
			// y
			// + getIconHeight() / 2);
			//
			// ((Graphics2D) g).setStroke(oldStroke);
			if (shouldDrawArrow()) {
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

	protected LineType getLineType() {
		return lineType;
	}

	protected void setLineType(LineType lineType) {
		this.lineType = lineType;
	}

	protected int getLineWidth() {
		return lineWidth*1;
	}

	protected void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}

}
