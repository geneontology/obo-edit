package org.oboedit.gui.filter;

import java.awt.Color;

import org.bbop.swing.ColorUtil;
import org.obo.util.HTMLUtil;
import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class ForegroundColorSpecField extends
	AbstractRendererSpecField<ColorProvider> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ForegroundColorSpecField.class);

	public static final ForegroundColorSpecField FIELD = new ForegroundColorSpecField();

	public ForegroundColorSpecField() {
	}

	public GeneralRendererSpecFieldEditor<ColorProvider> getEditor() {
		return new ColorSpecEditor();
	}

	public String getID() {
		return "foreground_color";
	}

	public String getName() {
		return "Foreground Color";
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
		Color c = value.getColor(selector, o);
		in.insert(0, "<font color='"
				+ ColorUtil.getHTMLCode(c) + "'>");
		in.append("</font>");
	}

	public boolean isLinkRenderer() {
		return true;
	}

	public boolean isObjectRenderer() {
		return true;
	}

}
