package org.oboedit.gui.filter;

public class ItalicSpecField extends AbstractRendererSpecField<Boolean> {

	public static final ItalicSpecField FIELD = new ItalicSpecField();

	public ItalicSpecField() {
	}
	
	public String getID() {
		return "italic";
	}

	public String getName() {
		return "Italic";
	}

	public Boolean merge(Boolean a, Boolean b) {
		return a || b;
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(Boolean value, StringBuffer in, Object o) {
		if (value.booleanValue()) {
			in.insert(0, "<i>");
			in.append("</i>");
		}
	}

	public GeneralRendererSpecFieldEditor<Boolean> getEditor() {
		return new EmptyBooleanEditor();
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}
}
