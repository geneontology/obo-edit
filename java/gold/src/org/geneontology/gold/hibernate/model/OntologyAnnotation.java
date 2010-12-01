package org.geneontology.gold.hibernate.model;

/**
 * 
 * @author Shahid Manzoor
 *
 */
public class OntologyAnnotation extends GOModel implements java.io.Serializable {

	private Ontology ontology;
	private String property;
	private String annotationValue;

	public OntologyAnnotation() {
		
		String keys[] = {"ontology", "property", "annotationValue"};
		
		this.initUniqueConstraintFields(OntologyAnnotation.class, keys);
		
	}

	public OntologyAnnotation(Ontology ontology, String property,
			String annotationValue) {
		this();
		this.ontology = ontology;
		this.property = property;
		this.annotationValue = annotationValue;
	}

	public Ontology getOntology() {
		return this.ontology;
	}

	public void setOntology(Ontology ontology) {
		this.ontology = ontology;
	}

	public String getProperty() {
		return this.property;
	}

	public void setProperty(String property) {
		this.property = property;
	}

	public String getAnnotationValue() {
		return this.annotationValue;
	}

	public void setAnnotationValue(String annotationValue) {
		this.annotationValue = annotationValue;
	}

}
