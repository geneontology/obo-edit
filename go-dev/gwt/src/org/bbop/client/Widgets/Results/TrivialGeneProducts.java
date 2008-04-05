package org.bbop.client.Widgets.Results;

import org.bbop.client.model.GO.GeneProduct;

import com.google.gwt.user.client.Window;

//
public class TrivialGeneProducts extends Trivial{

	// Add a string to the table;
	public void add (Object result){
		
		GeneProduct[] res = (GeneProduct[]) result;

		if( res.length > 0 ){					
			clearResults();
			for (int i = 0; i < res.length; i++) {
				GeneProduct n = res[i];
				this.setText(i, 0, n.getId());
				this.setText(i, 1, n.getLabel());
				this.setText(i, 2, n.getFullName());
			}
		}else{
			Window.alert("Unknown query.");
		}
	}
}
