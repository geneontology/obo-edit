package org.oboedit.gui.filter;

public class FontSizeSpecField extends AbstractRendererSpecField<Integer> {

	public static final FontSizeSpecField FIELD = new FontSizeSpecField();

	public FontSizeSpecField() {
	}
	
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

	public void renderHTML(Integer value, StringBuffer in, Object o) {
		in.insert(0, "<font size='" + value.intValue() + "'>");
		in.append("</font>");
	}

	public GeneralRendererSpecFieldEditor<Integer> getEditor() {
		return new IntegerSpecEditor();
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}
}
