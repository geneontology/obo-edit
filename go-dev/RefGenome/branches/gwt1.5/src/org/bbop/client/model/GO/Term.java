package org.bbop.client.model.GO;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;


public class Term extends Base implements IsSerializable {

	String url;

	// 
	public Term(String acc) {

		 super(acc);

		 url = "http://amigo.geneontology.org/cgi-bin/amigo/term-details.cgi?term=";
		 
	}
	
	public Term() {
		
	}

	// Wrap with id.
	public String getAcc(){ return this.getId(); }
	public void setAcc(String in_acc){ this.setId(in_acc); }

	// Wrap with label.
	public String getName(){ return this.getLabel(); }
	public void setName(String in_n){ this.setLabel(in_n); }

	public String getURL(){ return url + this.getAcc(); }
}
