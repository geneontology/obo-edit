package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class StrikeoutSpecField extends AbstractRendererSpecField<Boolean> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(StrikeoutSpecField.class);

	public static final StrikeoutSpecField FIELD = new StrikeoutSpecField();

	public StrikeoutSpecField() {
	}

	public String getID() {
		return "strikeout";
	}

	public String getName() {
		return "Strikeout";
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
			in.insert(0, "<strike>");
			in.append("</strike>");
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
