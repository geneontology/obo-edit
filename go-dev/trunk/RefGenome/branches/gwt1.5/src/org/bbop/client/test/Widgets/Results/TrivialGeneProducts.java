package org.bbop.client.test.Widgets.Results;

import org.bbop.client.model.GOGeneProduct;

import com.google.gwt.user.client.Window;

//
public class TrivialGeneProducts extends Trivial{

	// Add a string to the table;
	public void add (Object result){
		
		GOGeneProduct[] res = (GOGeneProduct[]) result;

		if( res.length > 0 ){					
			clearResults();
			for (int i = 0; i < res.length; i++) {
				GOGeneProduct n = res[i];
				this.setText(i, 0, n.getId());
				this.setHTML(i, 1, n.makeURL());
				this.setText(i, 2, n.getLabel());
				this.setText(i, 3, n.getFullName());
			}
		}else{
			Window.alert("Unknown query.");
		}
	}
}
