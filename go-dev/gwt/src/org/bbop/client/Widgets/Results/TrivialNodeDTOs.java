package org.bbop.client.Widgets.Results;

import org.bbop.client.model.NodeDTO;

import com.google.gwt.user.client.Window;

//
public class TrivialNodeDTOs extends Trivial{

	// Add a string to the table;
	public void add (Object result){
		
		NodeDTO[] res = (NodeDTO[]) result;

		if( res.length > 0 ){					
			clearResults();
			for (int i = 0; i < res.length; i++) {
				NodeDTO n = res[i];
				this.setText(i, 0, n.getLabel());
				this.setText(i, 1, n.getId());
				this.setText(i, 2, n.getSourceId());
			}
		}else{
			Window.alert("Unknown query.");
		}
	}
}
