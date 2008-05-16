package org.oboedit.gui;

import org.apache.log4j.*;

public class StringFieldAutocompleteModel extends FieldAutocompleteModel<String> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(StringFieldAutocompleteModel.class);

	public StringFieldAutocompleteModel() {
		super(String.class);
	}
	
	@Override
	protected boolean equals(String a, String b) {
		return a.equalsIgnoreCase(b);
	}

}
