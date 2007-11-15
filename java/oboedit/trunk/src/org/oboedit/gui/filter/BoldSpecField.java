package org.oboedit.gui.filter;

import org.bbop.swing.SwingUtil;
import org.obo.util.HTMLUtil;

public class BoldSpecField extends AbstractRendererSpecField<Boolean> {

	public static final BoldSpecField FIELD = new BoldSpecField();

	public BoldSpecField() {
	}
	
	public String getID() {
		return "bold";
	}

	public String getName() {
		return "Bold";
	}

	public Boolean merge(Boolean a, Boolean b) {
		return a || b;
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(Boolean value, StringBuffer in, Object o) {
		if (value.booleanValue()) {
			in.insert(0, "<b>");
			in.append("</b>");
		}
	}

	public GeneralRendererSpecFieldEditor<Boolean> getEditor() {
		return new EmptyBooleanEditor();
	}

	public boolean isLinkRenderer() {
		return false;
	}

	public boolean isObjectRenderer() {
		return true;
	}
}
