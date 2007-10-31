package org.oboedit.gui.filter;

public class FontFaceSpecField implements GeneralRendererSpecField<String> {

	public static final FontFaceSpecField FIELD = new FontFaceSpecField();

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
		in.insert(0, "<font face='" + value + "'>");
		in.append("</font>");
	}

	public GeneralRendererSpecFieldEditor<String> getEditor() {
		return new FontFaceEditor();
	}

}
