package org.oboedit.gui.event;

import java.util.EventObject;

import org.oboedit.gui.Selection;

import org.apache.log4j.*;

public class SelectionEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SelectionEvent.class);
	protected Selection selection;
	protected String option;
	
	
	
	public String getOption() {
		return option;
	}

	public SelectionEvent(Object o, Selection selection) {
		super(o);
		this.selection = selection;
	}
	
	public SelectionEvent(Selection selection) {
		this(selection.getComponent(), selection);
	}
	
	/**
	 * Enables the passing of a selection event, but with an instruction not to reload the UI of other listening components.
	 *  
	 * @param source
	 * @param current
	 * @param option determines whether the UI of listening components will be reloaded. 
	 */
	public SelectionEvent(Object source, Selection current, String option) {
		super(source);
		this.selection = current;
		this.option = option;
		
	}

	public Selection getSelection() {
		return selection;
	}
}
