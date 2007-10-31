package org.oboedit.gui.filter;

public class StrikeoutSpecField implements GeneralRendererSpecField<Boolean> {

	public static final StrikeoutSpecField FIELD = new StrikeoutSpecField();

	public String getID() {
		return "strikeout";
	}

	public String getName() {
		return "Strikeout";
	}

	public Boolean merge(Boolean a, Boolean b) {
		return a || b;
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(Boolean value, StringBuffer in) {
		if (value.booleanValue()) {
			in.insert(0, "<strike>");
			in.append("</strike>");
		}
	}

	public GeneralRendererSpecFieldEditor<Boolean> getEditor() {
		return new EmptyBooleanEditor();
	}

}
