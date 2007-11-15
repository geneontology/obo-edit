package org.oboedit.gui.filter;

import org.obo.datamodel.LinkedObject;
import org.oboedit.gui.HTMLNodeLabelProvider;

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

	public void renderHTML(String value, StringBuffer in, Object obj) {
		int replaceIndex = value.indexOf(REPLACE_SEQ);
		if (replaceIndex >= 0) {
			in.insert(0, value.substring(0, replaceIndex));
			in.append(value.substring(replaceIndex + REPLACE_SEQ.length(),
					value.length()));
		}
		if (obj instanceof LinkedObject) {
			LinkedObject lo = (LinkedObject) obj;
			String s = HTMLNodeLabelProvider.resolveHTMLExpression(in
					.toString(), lo);
			in.replace(0, in.length(), s);
		}
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
