package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class FontFaceSpecField extends AbstractRendererSpecField<String> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FontFaceSpecField.class);

	public static final FontFaceSpecField FIELD = new FontFaceSpecField();

	public FontFaceSpecField() {
	}

	public String getID() {
		return "font_face";
	}

	public String getName() {
		return "Font Face";
	}

	public String merge(FilteredRenderable fr, String a, String b, Object o) {
		if (a == null)
			return b;
		else if (b == null)
			return a;
		else
			return b;
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(FilteredRenderable fr, String value,
			StringBuffer in, Object o) {
		in.insert(0, "<style type='text/css>\n* {font-family: " + value
				+ "; }\n</style><font face='" + value + "'>");
		in.append("</font>");
	}

	public GeneralRendererSpecFieldEditor<String> getEditor() {
		return new FontFaceEditor();
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}
}
