package org.oboedit.gui;

public class StringFieldAutocompleteModel extends FieldAutocompleteModel<String> {

	public StringFieldAutocompleteModel() {
		super(String.class);
	}
	
	@Override
	protected boolean equals(String a, String b) {
		return a.equalsIgnoreCase(b);
	}

}
