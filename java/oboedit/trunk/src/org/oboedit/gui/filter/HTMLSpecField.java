package org.oboedit.gui.filter;

public class HTMLSpecField extends AbstractRendererSpecField<String> {

	public static final HTMLSpecField FIELD = new HTMLSpecField();
	protected static final String REPLACE_SEQ = "$term$";

	public HTMLSpecField() {
	}
	
	public String getID() {
		return "html_code";
	}

	public String getName() {
		return "HTML Code";
	}

	public String merge(String a, String b) {
		return a.replace(REPLACE_SEQ, b);
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(String value, StringBuffer in) {
		in.insert(0, value.substring(0, value.indexOf(REPLACE_SEQ)));
		in.append(value.substring(value.indexOf(REPLACE_SEQ)
				+ REPLACE_SEQ.length(), value.length()));
	}

	public GeneralRendererSpecFieldEditor<String> getEditor() {
		return new HTMLSpecEditor();
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}
}
