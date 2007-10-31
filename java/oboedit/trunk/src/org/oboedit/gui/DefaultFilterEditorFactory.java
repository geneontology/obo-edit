package org.oboedit.gui;

import org.obo.filters.*;
import org.oboedit.gui.widget.CompoundFilterEditor;
import org.oboedit.gui.widget.LinkFilterPanel;
import org.oboedit.gui.widget.ObjectFilterPanel;

public class DefaultFilterEditorFactory implements FilterEditorFactory {

	protected FilterEditor compoundEditor = new CompoundFilterEditor();
	protected FilterEditor objectFilterEditor = new ObjectFilterPanel();
	protected FilterEditor linkFilterEditor = new LinkFilterPanel();

	public DefaultFilterEditorFactory() {
		// TODO Auto-generated constructor stub
	}
	
	public FilterEditor getFilterEditor(Filter filter) {
		if (filter instanceof CompoundFilter)
			return compoundEditor;
		else if (filter instanceof ObjectFilter)
			return objectFilterEditor;
		else if (filter instanceof LinkFilter)
			return linkFilterEditor;
		else
			throw new IllegalArgumentException();
	}
}
