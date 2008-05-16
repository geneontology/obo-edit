package org.oboedit.gui.event;

import java.util.EventObject;

import org.oboedit.gui.Selection;

import org.apache.log4j.*;

public class SelectionEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SelectionEvent.class);
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
