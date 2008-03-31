package org.bbop.client.Widgets;

import com.google.gwt.user.client.ui.FlexTable;


//
public class TrivialResultsTable extends FlexTable{

	
	public TrivialResultsTable (){

		super();
	}

	// Wipe table.
	public void reset (String str){
		for( int i = this.getRowCount(); i > 0; i-- ){
			this.removeRow(0);
		}
		this.setText(this.getRowCount(), 0, str);
	}
	
	// Completely wipe table.
	public void clear () {
		for( int i = this.getRowCount(); i > 0; i-- ){
			this.removeRow(0);
		}
	}
	
	// Add a string to the table;
	public void add (String str){
		this.setText(this.getRowCount(), 0, str);
	}	
}
