package org.oboedit.gui.filter;

import org.oboedit.gui.LineTypes;

import net.infonode.gui.icon.button.ArrowIcon;

public class LineTypeSpecField implements GeneralRendererSpecField<LineTypes> {

	public static final LineTypeSpecField FIELD = new LineTypeSpecField();

	public String getID() {
		return "line_type";
	}

	public String getName() {
		return "Line Type";
	}

	public int getHTMLType() {
		return NON_HTML;
	}

	public void renderHTML(Integer value, StringBuffer in) {
		throw new UnsupportedOperationException();
	}

	public GeneralRendererSpecFieldEditor<LineTypes> getEditor() {
		return new DropdownListSpecEditor<LineTypes>(LineTypes.values());
	}

	public boolean isLinkRenderer() {
		return true;
	}

	public boolean isObjectRenderer() {
		return false;
	}

	public LineTypes merge(LineTypes a, LineTypes b) {
		return a;
	}

	public void renderHTML(LineTypes value, StringBuffer in) {
		throw new UnsupportedOperationException();
	}
}
