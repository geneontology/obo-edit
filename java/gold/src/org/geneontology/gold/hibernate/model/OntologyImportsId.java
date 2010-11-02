package org.geneontology.gold.hibernate.model;

// Generated Nov 2, 2010 3:53:51 PM by Hibernate Tools 3.4.0.Beta1

/**
 * OntologyImportsId generated by hbm2java
 */
public class OntologyImportsId implements java.io.Serializable {

	private String ontology;
	private String importsOntology;

	public OntologyImportsId() {
	}

	public OntologyImportsId(String ontology, String importsOntology) {
		this.ontology = ontology;
		this.importsOntology = importsOntology;
	}

	public String getOntology() {
		return this.ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

	public String getImportsOntology() {
		return this.importsOntology;
	}

	public void setImportsOntology(String importsOntology) {
		this.importsOntology = importsOntology;
	}

	public boolean equals(Object other) {
		if ((this == other))
			return true;
		if ((other == null))
			return false;
		if (!(other instanceof OntologyImportsId))
			return false;
		OntologyImportsId castOther = (OntologyImportsId) other;

		return ((this.getOntology() == castOther.getOntology()) || (this
				.getOntology() != null && castOther.getOntology() != null && this
				.getOntology().equals(castOther.getOntology())))
				&& ((this.getImportsOntology() == castOther
						.getImportsOntology()) || (this.getImportsOntology() != null
						&& castOther.getImportsOntology() != null && this
						.getImportsOntology().equals(
								castOther.getImportsOntology())));
	}

	public int hashCode() {
		int result = 17;

		result = 37 * result
				+ (getOntology() == null ? 0 : this.getOntology().hashCode());
		result = 37
				* result
				+ (getImportsOntology() == null ? 0 : this.getImportsOntology()
						.hashCode());
		return result;
	}

}
