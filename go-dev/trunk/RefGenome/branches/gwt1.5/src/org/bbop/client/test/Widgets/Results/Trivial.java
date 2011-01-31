package org.bbop.client.test.Widgets.Results;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.FlexTable;


//
public class Trivial extends FlexTable{

	
	public Trivial (){

		super();
	}

	protected void clearResults (){
		for( int i = this.getRowCount(); i > 0; i-- ){
			this.removeRow(0);
		}
	}
	
	// Wipe table.
	public void reset (String str){
		clearResults();
		this.setText(this.getRowCount(), 0, str);
	}
	
	// Add a string to the table;
	public void add (Object result){

		Object[] res = (Object[]) result;

		if( res.length > 0 ){					
			clearResults();
			for (int i = 0; i < res.length; i++) {
				String str = res[i].toString();
				this.setText(i, 0, str);
			}
		}else{
			Window.alert("Unknown query.");
		}
	}
}
