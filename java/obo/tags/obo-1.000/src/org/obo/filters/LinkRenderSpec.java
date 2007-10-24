package org.obo.filters;

import java.awt.Color;

public class LinkRenderSpec implements RenderSpec {

	public static final int SOLID_LINE = 0;
	public static final int WAVY_LINE = 1;
	public static final int DASHED_LINE = 2;

	protected Color linkColor;
	protected int lineWidth;
	protected int lineType;

	public LinkRenderSpec() {
		clear();
	}

	@Override
	public String toString() {
		String out = "";
		if (linkColor != null)
			out += "color = " + linkColor;
		if (lineWidth != -1) {
			if (!out.equals(""))
				out += ", ";
			out += "line width = " + lineWidth;
		}
		if (lineType != -1) {
			if (!out.equals(""))
				out += ", ";
			if (lineType == SOLID_LINE)
				out += "line type = SOLID";
			else if (lineType == WAVY_LINE)
				out += "line type = WAVY";
			else if (lineType == DASHED_LINE)
				out += "line type = DASHED";
		}
		return out;
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (Exception ex) {
			return null;
		}
	}

	public void clear() {
		linkColor = null;
		lineWidth = -1;
		lineType = -1;
	}

	public void setLineType(int lineType) {
		this.lineType = lineType;
	}

	public int getLineType() {
		return lineType;
	}

	public void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}

	public int getLineWidth() {
		return lineWidth;
	}

	public void setLinkColor(Color linkColor) {
		this.linkColor = linkColor;
	}

	public Color getLinkColor() {
		return linkColor;
	}

	public void merge(RenderSpec s) {
		if (s == null)
			return;
		if (!(s instanceof LinkRenderSpec))
			throw new IllegalArgumentException();
		LinkRenderSpec spec = (LinkRenderSpec) s;
		if (lineWidth == -1)
			lineWidth = spec.getLineWidth();
		if (lineType == -1)
			lineType = spec.getLineType();
		if (linkColor == null)
			linkColor = spec.getLinkColor();
	}

}
