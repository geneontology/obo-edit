package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class LineWidthSpecField extends AbstractRendererSpecField<Integer> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LineWidthSpecField.class);

	public static final LineWidthSpecField FIELD = new LineWidthSpecField();

	public LineWidthSpecField() {
	}

	public String getID() {
		return "line_width";
	}

	public String getName() {
		return "Line Width";
	}

	public Integer merge(FilteredRenderable fr, Integer a, Integer b, Object o) {
		return a + b;
	}

	public int getHTMLType() {
		return NON_HTML;
	}

	public void renderHTML(FilteredRenderable fr, Integer value,
			StringBuffer in, Object o) {
		throw new UnsupportedOperationException();
	}

	public GeneralRendererSpecFieldEditor<Integer> getEditor() {
		return new IntegerSpecEditor(1);
	}

	public boolean isLinkRenderer() {
		return true;
	}

	public boolean isObjectRenderer() {
		return false;
	}
}
