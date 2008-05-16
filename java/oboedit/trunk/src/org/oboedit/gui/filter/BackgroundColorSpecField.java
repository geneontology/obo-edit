package org.oboedit.gui.filter;

import org.bbop.swing.ColorUtil;
import org.obo.util.HTMLUtil;
import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class BackgroundColorSpecField extends
	AbstractRendererSpecField<ColorProvider> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BackgroundColorSpecField.class);

	public static final BackgroundColorSpecField FIELD = new BackgroundColorSpecField();

	public BackgroundColorSpecField() {
	}

	public GeneralRendererSpecFieldEditor<ColorProvider> getEditor() {
		return new ColorSpecEditor();
	}

	public String getID() {
		return "background_color";
	}

	public String getName() {
		return "Background Color";
	}

	public ColorProvider merge(FilteredRenderable selector, ColorProvider a,
			ColorProvider b, Object o) {
		return new ConfiguredColor(ColorUtil.mergeColors(a
				.getColor(selector, o), a.getColor(selector, o)), true);
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(FilteredRenderable selector, ColorProvider value,
			StringBuffer in, Object o) {
		in.insert(0, "<span style='background-color: "
				+ ColorUtil.getHTMLCode(value.getColor(selector, o)) + ";'>");
		in.append("</span>");
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}

}
