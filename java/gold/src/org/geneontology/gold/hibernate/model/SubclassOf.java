package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class SubclassOf implements Serializable {

	private Cls cls;
	private Cls superCls;
	private String ontology;
	
	
	public SubclassOf() {
	}
	
	public SubclassOf(Cls cls, Cls superCls, String ontology) {
		this.cls = cls;
		this.superCls = superCls;
		this.ontology = ontology;
	}
	
	
	public Cls getCls() {
		return cls;
	}
	public void setCls(Cls cls) {
		this.cls = cls;
	}
	public Cls getSuperCls() {
		return superCls;
	}
	public void setSuperCls(Cls superCls) {
		this.superCls = superCls;
	}
	public String getOntology() {
		return ontology;
	}
	public void setOntology(String ontology) {
		this.ontology = ontology;
	}
	
	
	
}
