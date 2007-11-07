package org.oboedit.gui.filter;

public class FontFaceSpecField extends AbstractRendererSpecField<String> {

	public static final FontFaceSpecField FIELD = new FontFaceSpecField();

	public FontFaceSpecField() {
	}
	
	public String getID() {
		return "font_face";
	}

	public String getName() {
		return "Font Face";
	}

	public String merge(String a, String b) {
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

	public void renderHTML(String value, StringBuffer in) {
		in.insert(0, "<style type='text/css>\n* {font-family: "+value+"; }\n</style><font face='" + value + "'>");
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
