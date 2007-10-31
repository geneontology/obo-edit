package org.oboedit.gui.filter;

public class FontSizeSpecField implements GeneralRendererSpecField<Integer> {

	public static final FontSizeSpecField FIELD = new FontSizeSpecField();

	public String getID() {
		return "font_size";
	}

	public String getName() {
		return "Font Size";
	}

	public Integer merge(Integer a, Integer b) {
		return a + b;
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(Integer value, StringBuffer in) {
		in.insert(0, "<font size='" + value.intValue() + "'>");
		in.append("</font>");
	}

	public GeneralRendererSpecFieldEditor<Integer> getEditor() {
		return new IntegerSpecEditor();
	}

}
