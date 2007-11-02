package org.oboedit.gui.filter;

import org.bbop.swing.SwingUtil;
import org.obo.util.HTMLUtil;

public class BackgroundColorSpecField implements
		GeneralRendererSpecField<ConfiguredColor> {

	public static final BackgroundColorSpecField FIELD = new BackgroundColorSpecField();

	public GeneralRendererSpecFieldEditor<ConfiguredColor> getEditor() {
		return new ColorSpecEditor();
	}

	public String getID() {
		return "background_color";
	}

	public String getName() {
		return "Background Color";
	}

	public ConfiguredColor merge(ConfiguredColor a, ConfiguredColor b) {
		return ConfiguredColor.merge(a, b);
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(ConfiguredColor value, StringBuffer in) {
		in.insert(0, "<span style='background-color: "
				+ SwingUtil.getHTMLCode(value.getColor()) + ";'>");
		in.append("</span>");
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}

}
