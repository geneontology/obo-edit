package org.geneontology.gold.hibernate.model;

// Generated Oct 22, 2010 9:25:51 AM by Hibernate Tools 3.4.0.Beta1

/**
 * OntologyAnnotation generated by hbm2java
 */
public class OntologyAnnotation implements java.io.Serializable {

	private OntologyAnnotationId id;

	public OntologyAnnotation() {
	}

	public OntologyAnnotation(OntologyAnnotationId id) {
		this.id = id;
	}

	public OntologyAnnotationId getId() {
		return this.id;
	}

	public void setId(OntologyAnnotationId id) {
		this.id = id;
	}

}
