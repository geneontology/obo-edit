package org.bbop.client.model.GO;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;


public class GeneProduct extends Base implements IsSerializable {

	String full_name;
	String url;
	
	//
	public GeneProduct() {

		 super("null_gene_product");

		 full_name = "";
		 url = "http://amigo.geneontology.org/cgi-bin/amigo/gp-details.cgi?gp=";
		 
	}

	//
	public GeneProduct(String id) {

		 super(id);

		 full_name = "";
		 url = "http://amigo.geneontology.org/cgi-bin/amigo/gp-details.cgi?gp=";
		 
	}

	
	// Wrap with id.
	public String getId(){ return this.getId(); }
	public void setId(String in_id){ this.setId(in_id); }

	// Wrap with label.
	public String getSymbol(){ return this.getLabel(); }
	public void setSymbol(String in_sym){ this.setLabel(in_sym); }

	public String getFullName(){ return this.full_name; }
	public void setFullName(String in_fn){ this.full_name = in_fn; }
	
	public String getURL(){ return url + this.getId(); }
}
