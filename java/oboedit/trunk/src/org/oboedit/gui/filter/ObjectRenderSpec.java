package org.oboedit.gui.filter;

import java.awt.Color;


public class ObjectRenderSpec implements RenderSpec {

	protected Color foregroundColor;
	protected String fontName;
	protected int fontSize;
	protected boolean isBold;
	protected boolean isItalic;
	protected boolean isUnderlined;

	public ObjectRenderSpec() {
		clear();
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
		foregroundColor = null;
		fontName = null;
		fontSize = -1;
		isBold = false;
		isItalic = false;
		isUnderlined = false;
	}

	public void setUnderlined(boolean isUnderlined) {
		this.isUnderlined = isUnderlined;
	}

	public boolean getUnderlined() {
		return isUnderlined;
	}

	public boolean getItalic() {
		return isItalic;
	}

	public void setItalic(boolean isItalic) {
		this.isItalic = isItalic;
	}

	public boolean getBold() {
		return isBold;
	}

	public void setBold(boolean isBold) {
		this.isBold = isBold;
	}

	public void setFontSize(int fontSize) {
		this.fontSize = fontSize;
	}

	public int getFontSize() {
		return fontSize;
	}

	public void setFontName(String fontName) {
		this.fontName = fontName;
	}

	public String getFontName() {
		return fontName;
	}

	public void setForegroundColor(Color foregroundColor) {
		this.foregroundColor = foregroundColor;
	}

	public Color getForegroundColor() {
		return foregroundColor;
	}

	@Override
	public String toString() {
		String out = "";
		if (foregroundColor != null)
			out += "foreground = " + foregroundColor;
		if (fontName != null) {
			if (!out.equals(""))
				out += ", ";
			out += "font = " + fontName;
		}
		if (fontSize >= 0) {
			if (!out.equals(""))
				out += ", ";
			out += "font size = " + fontSize;
		}
		if (isUnderlined) {
			if (!out.equals(""))
				out += ", ";
			out += "underline";
		}
		if (isItalic) {
			if (!out.equals(""))
				out += ", ";
			out += "italic";
		}
		if (isBold) {
			if (!out.equals(""))
				out += ", ";
			out += "bold";
		}
		return out;
	}

	public RenderSpec merge(RenderSpec s) {
		if (s == null)
			return this;
		if (!(s instanceof ObjectRenderSpec))
			throw new IllegalArgumentException();

		ObjectRenderSpec spec = (ObjectRenderSpec) s;

		if (!getUnderlined())
			setUnderlined(spec.getUnderlined());
		if (!getBold())
			setBold(spec.getBold());
		if (!getItalic())
			setItalic(spec.getItalic());
		if (getFontSize() < 0)
			setFontSize(spec.getFontSize());
		if (getFontName() == null)
			setFontName(spec.getFontName());
		if (getForegroundColor() == null)
			setForegroundColor(spec.getForegroundColor());
		return this;
	}
}
