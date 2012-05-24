package org.oboedit.gui.filter;

import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class ItalicSpecField extends AbstractRendererSpecField<Boolean> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ItalicSpecField.class);

	public static final ItalicSpecField FIELD = new ItalicSpecField();

	public ItalicSpecField() {
	}

	public String getID() {
		return "italic";
	}

	public String getName() {
		return "Italic";
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
			in.insert(0, "<i>");
			in.append("</i>");
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
