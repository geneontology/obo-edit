package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class InferredSubclassOf extends InferredRelationship implements Serializable {

	public InferredSubclassOf() {
		super();
	}

	public InferredSubclassOf(Cls cls, Cls targetCls, Relation relation,
			Boolean isDirect, Boolean isReflexive, Ontology ontology,
			String quantifier) {
		super(cls, targetCls, relation, isDirect, isReflexive, ontology, quantifier);
	}

	
	
	/*private Cls cls;
	private Cls targetCls;
	private Boolean isDirect;
	private Boolean isReflexive;
	private Ontology ontology;

	public InferredSubclassOf() {
		String uniqueKeys[] = {"cls", "targetCls",  "ontology"};
		this.initUniqueConstraintFields(InferredRelationship.class, uniqueKeys);
	}

	public InferredSubclassOf(Cls cls, Cls targetCls,
			Boolean isDirect, Boolean isReflexive,
			Ontology ontology) {
		this();
		this.cls = cls;
		this.targetCls = targetCls;
		this.isDirect = isDirect;
		this.isReflexive = isReflexive;
		this.ontology = ontology;
	}

	public Cls getCls() {
		return this.cls;
	}

	public void setCls(Cls cls) {
		this.cls = cls;
	}

	public Cls getTargetCls() {
		return this.targetCls;
	}

	public void setTargetCls(Cls targetCls) {
		this.targetCls = targetCls;
	}

	public Boolean getIsDirect() {
		return this.isDirect;
	}

	public void setIsDirect(Boolean isDirect) {
		this.isDirect = isDirect;
	}

	public Boolean getIsReflexive() {
		return this.isReflexive;
	}

	public void setIsReflexive(Boolean isReflexive) {
		this.isReflexive = isReflexive;
	}

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}*/
	
	
}
