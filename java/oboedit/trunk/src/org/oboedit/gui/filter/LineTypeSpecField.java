package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.LineType;

import net.infonode.gui.icon.button.ArrowIcon;

import org.apache.log4j.*;

public class LineTypeSpecField extends AbstractRendererSpecField<LineType> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LineTypeSpecField.class);

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

	public void renderHTML(FilteredRenderable fr, LineType value,
			StringBuffer in, Object o) {
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

	public LineType merge(FilteredRenderable fr, LineType a, LineType b, Object o) {
		return a;
	}
}
