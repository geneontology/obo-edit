package org.geneontology.gold.hibernate.model;

import java.io.Serializable;

public class InferredAllSomeRelationship extends InferredRelationship implements
		Serializable {

	public InferredAllSomeRelationship() {
		super();
	}

	public InferredAllSomeRelationship(Cls cls, Cls targetCls,
			Relation relation, Boolean isDirect, Boolean isReflexive,
			Ontology ontology, String quantifier) {
		super(cls, targetCls, relation, isDirect, isReflexive, ontology,
				quantifier);
	}

}
