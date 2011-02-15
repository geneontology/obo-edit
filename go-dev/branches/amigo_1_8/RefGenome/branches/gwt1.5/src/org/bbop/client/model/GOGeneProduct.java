package org.bbop.client.model;

import java.util.ArrayList;
import java.util.List;



import com.google.gwt.user.client.rpc.IsSerializable;
import com.google.gwt.user.client.ui.Hyperlink;


public class GOGeneProduct extends GOBase implements IsSerializable {

	String full_name;
	String url;
	
	//
	public GOGeneProduct() {

		 super("null_gene_product");

		 full_name = "";
		 url = "http://amigo.geneontology.org/cgi-bin/amigo/gp-details.cgi?gp=";
		 
	}

	//
	public GOGeneProduct(String id) {

		 super(id);

		 full_name = "";
		 url = "http://amigo.geneontology.org/cgi-bin/amigo/gp-details.cgi?gp=";
		 
	}

	
	// Wrap with id.
	//public String getId(){ return this.getId(); }
	//public void setId(String in_id){ this.setId(in_id); }

	// Wrap with label.
	public String getSymbol(){ return getLabel(); }
	public void setSymbol(String in_sym){ setLabel(in_sym); }

	public String getFullName(){ return full_name; }
	public void setFullName(String in_fn){ full_name = in_fn; }
	
	public String getURL(){ return url + this.getId(); }
	
	public String makeURL(){
		
		String new_url;
		//Hyperlink link = new Hyperlink(getId(), url + getId());
		//new_url = link.getHTML();
		new_url = "<a title=\"external link to " + getId() + "\" href=\"" + url + getId() + "\">" + getId() + "</a>";
		
		return new_url;
	}
}
