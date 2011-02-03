package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.GOModel;
import org.geneontology.gold.hibernate.model.Relation;

public class ExtensionExpression extends GOModel implements Serializable {

	private String id;
	private String relation;
	private String cls;
	
	private Relation relationObj;
	private Cls clsObj;

	
	public ExtensionExpression(){
		String keys[] = {"id"};
		this.initUniqueConstraintFields(ExtensionExpression.class, keys);
	}
	
	
	public ExtensionExpression(String id, String relation, String cls) {
		this();
		this.id = id;
		this.relation = relation;
		this.cls = cls;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getRelation() {
		return relation;
	}
	public void setRelation(String relation) {
		this.relation = relation;
	}
	public String getCls() {
		return cls;
	}
	public void setCls(String cls) {
		this.cls = cls;
	}
	public Relation getRelationObj() {
		if(relationObj == null)
			relationObj = (Relation)getHibernateObject(Relation.class, "id", getRelation());
		
		return relationObj;
	}
	public void setRelationObj(Relation relationObj) {
		this.relationObj = relationObj;
	}
	public Cls getClsObj() {
		if(clsObj == null)
			clsObj = (Cls) getHibernateObject(Cls.class, "id", getCls());
		
		
		return clsObj;
	}
	public void setClsObj(Cls clsObj) {
		this.clsObj = clsObj;
	}
	
	
	
}
