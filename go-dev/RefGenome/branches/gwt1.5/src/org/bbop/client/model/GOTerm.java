package org.bbop.client.model;

import java.util.ArrayList;
import java.util.List;



import com.google.gwt.user.client.rpc.IsSerializable;


public class GOTerm extends GOBase implements IsSerializable {

	String url;

	// 
	public GOTerm(String acc) {

		 super(acc);

		 url = "http://amigo.geneontology.org/cgi-bin/amigo/term-details.cgi?term=";
		 
	}

	// Wrap with id.
	public String getAcc(){ return this.getId(); }
	public void setAcc(String in_acc){ this.setId(in_acc); }

	// Wrap with label.
	public String getName(){ return this.getLabel(); }
	public void setName(String in_n){ this.setLabel(in_n); }

	public String getURL(){ return url + this.getAcc(); }
}
