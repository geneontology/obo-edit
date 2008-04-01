package org.bbop.client.Widgets.Results;

import com.google.gwt.user.client.Window;

//
public class TrivialStrings extends Trivial{

	// Add a string to the table;
	public void add (Object result){

		String[] res = (String[]) result;
		
		if( res.length > 0 ){					
			clearResults();
			for (int i = 0; i < res.length; i++) {
				String str = res[i];
				this.setText(i, 0, str);
			}
		}else{
			Window.alert("Unknown query.");
		}
	}
}
