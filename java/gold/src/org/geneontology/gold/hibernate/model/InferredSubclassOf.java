package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class InferredSubclassOf extends InferredRelationship implements Serializable {

	public InferredSubclassOf() {
		super();
	}

	public InferredSubclassOf(String cls, String targetCls, String relation,
			Boolean isDirect, Boolean isReflexive, String ontology,
			String quantifier) {
		super(cls, targetCls, relation, isDirect, isReflexive, ontology, quantifier);
	}

	
	
	
	
}
