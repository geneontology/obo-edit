package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class UnderlineSpecField extends AbstractRendererSpecField<Boolean> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UnderlineSpecField.class);

	public static final UnderlineSpecField FIELD = new UnderlineSpecField();

	public UnderlineSpecField() {
	}

	public String getID() {
		return "underline";
	}

	public String getName() {
		return "Underline";
	}

	public Boolean merge(FilteredRenderable fr, Boolean a, Boolean b, Object o) {
		return a || b;
	}

	public int getHTMLType() {
		return HTML;
	}

	public void renderHTML(FilteredRenderable fr, Boolean value,
			StringBuffer in, Object o) {
		if (value.booleanValue()) {
			in.insert(0, "<u>");
			in.append("</u>");
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
