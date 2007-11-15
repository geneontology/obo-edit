package org.oboedit.gui.filter;

public class LineWidthSpecField extends AbstractRendererSpecField<Integer> {

	public static final LineWidthSpecField FIELD = new LineWidthSpecField();

	public LineWidthSpecField() {
	}
	
	public String getID() {
		return "line_width";
	}

	public String getName() {
		return "Line Width";
	}

	public Integer merge(Integer a, Integer b) {
		return a + b;
	}

	public int getHTMLType() {
		return NON_HTML;
	}

	public void renderHTML(Integer value, StringBuffer in, Object o) {
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
