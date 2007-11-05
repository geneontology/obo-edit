package org.oboedit.gui.filter;

import org.oboedit.gui.LineType;

import net.infonode.gui.icon.button.ArrowIcon;

public class LineTypeSpecField extends AbstractRendererSpecField<LineType> {

	public static final LineTypeSpecField FIELD = new LineTypeSpecField();

	public LineTypeSpecField() {
	}
	
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

	public GeneralRendererSpecFieldEditor<LineType> getEditor() {
		return new DropdownListSpecEditor<LineType>(LineType.values());
	}

	public boolean isLinkRenderer() {
		return true;
	}

	public boolean isObjectRenderer() {
		return false;
	}

	public LineType merge(LineType a, LineType b) {
		return a;
	}

	public void renderHTML(LineType value, StringBuffer in) {
		throw new UnsupportedOperationException();
	}
}
