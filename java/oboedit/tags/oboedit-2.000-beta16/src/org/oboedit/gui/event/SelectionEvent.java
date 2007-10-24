package org.oboedit.gui.event;

import java.util.EventObject;

import org.oboedit.gui.Selection;

public class SelectionEvent extends EventObject {
	protected Selection selection;
	
	public SelectionEvent(Object o, Selection selection) {
		super(o);
		this.selection = selection;
	}
	
	public SelectionEvent(Selection selection) {
		this(selection.getComponent(), selection);
	}
	
	public Selection getSelection() {
		return selection;
	}
}
