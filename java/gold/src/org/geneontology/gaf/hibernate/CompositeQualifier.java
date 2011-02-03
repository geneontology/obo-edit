package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.GOModel;

public class CompositeQualifier extends GOModel implements Serializable {

	private String id;
	private String qualifierObj;
	
	public CompositeQualifier(){
		String keys[] = {"id", "qualifierObj"};
		this.initUniqueConstraintFields(CompositeQualifier.class, keys);
	}

	public CompositeQualifier(String id, String qualifierObj) {
		this();
		this.id = id;
		this.qualifierObj = qualifierObj;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getQualifierObj() {
		return qualifierObj;
	}

	public void setQualifierObj(String qualifierObj) {
		this.qualifierObj = qualifierObj;
	}
	
}
