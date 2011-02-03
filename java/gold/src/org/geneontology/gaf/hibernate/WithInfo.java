package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.GOModel;

public class WithInfo extends GOModel implements Serializable {

	private String id;
	private String withXref;
	
	public WithInfo(){
		String keys[] = {"id", "withXref"};
		this.initUniqueConstraintFields(WithInfo.class, keys);
	}

	public WithInfo(String id, String withXref) {
		this();
		this.id = id;
		this.withXref = withXref;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getWithXref() {
		return withXref;
	}

	public void setWithXref(String withXref) {
		this.withXref = withXref;
	}


	
	
}
