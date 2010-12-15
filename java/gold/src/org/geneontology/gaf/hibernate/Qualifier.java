package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.GOModel;

public class Qualifier extends GOModel implements Serializable {

	private String qualifierExpression;
	private String cls; //TODO: TBD is it Cls from gold
	
	public Qualifier(){
		String keys[] = {"qualifierExpression", "cls"};
		this.initUniqueConstraintFields(Qualifier.class, keys);
	}

	
	
	public Qualifier(String qualifierExpression, String cls) {
		this();
		this.qualifierExpression = qualifierExpression;
		this.cls = cls;
	}



	public String getQualifierExpression() {
		return qualifierExpression;
	}

	public void setQualifierExpression(String qualifierExpression) {
		this.qualifierExpression = qualifierExpression;
	}

	public String getCls() {
		return cls;
	}

	public void setCls(String cls) {
		this.cls = cls;
	}
	
	
	
	
}
