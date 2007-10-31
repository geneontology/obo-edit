package org.oboedit.gui.filter;

import org.bbop.swing.SwingUtil;
import org.obo.util.HTMLUtil;

public class ForegroundColorSpecField implements GeneralRendererSpecField<ConfiguredColor> {

	public static final ForegroundColorSpecField FIELD = new ForegroundColorSpecField();
	
	public ForegroundColorSpecField() {
	}
	
	public GeneralRendererSpecFieldEditor<ConfiguredColor> getEditor() {
		return new ColorSpecEditor();
	}

	public String getID() {
		return "foreground_color";
	}

	public String getName() {
		return "Foreground Color";
	}

	public ConfiguredColor merge(ConfiguredColor a, ConfiguredColor b) {
		return ConfiguredColor.merge(a, b);
	}
	
	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(ConfiguredColor value, StringBuffer in) {
		in.insert(0, "<font color='"+SwingUtil.getHTMLCode(value.getColor())+"'>");		
		in.append("</font>");
	}

}
