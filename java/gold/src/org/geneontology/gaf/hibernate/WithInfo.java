package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.GOModel;

public class WithInfo extends GOModel implements Serializable {

	private String withExpression;
	private String withXref;
	
	public WithInfo(){
		String keys[] = {"withExpression", "withXref"};
		this.initUniqueConstraintFields(WithInfo.class, keys);
	}

	public WithInfo(String withExpression, String withXref) {
		super();
		this.withExpression = withExpression;
		this.withXref = withXref;
	}

	public String getWithExpression() {
		return withExpression;
	}

	public void setWithExpression(String withExpression) {
		this.withExpression = withExpression;
	}

	public String getWithXref() {
		return withXref;
	}

	public void setWithXref(String withXref) {
		this.withXref = withXref;
	}
	
}
