package org.geneontology.gold.hibernate.model;


/**
 * 
 * @author Shahid Manzoor
 *
 */
public class SubrelationOf extends GOModel implements java.io.Serializable {

	private String relation;
	private String superRelation;
	private String ontology;

	private Relation relationObject;
	private Relation superRelationObject;
	private Ontology ontologyObject;
	
	public SubrelationOf() {
		String uniqueKeys[] = {"relation", "superRelation", "ontology"};
		this.initUniqueConstraintFields(SubrelationOf.class, uniqueKeys);
	}

	public SubrelationOf(String relation, String superRelation,
			String ontology) {
		this();
		this.relation = relation;
		this.superRelation = superRelation;
		this.ontology = ontology;
	}

	public String getRelation() {
		return relation;
	}

	public void setRelation(String relation) {
		this.relation = relation;
	}

	public String getSuperRelation() {
		return superRelation;
	}

	public void setSuperRelation(String superRelation) {
		this.superRelation = superRelation;
	}

	public String getOntology() {
		return ontology;
	}

	public void setOntology(String ontology) {
		this.ontology = ontology;
	}

	

	public Relation getRelationObject() {
		if(relationObject == null && relationObject != null){
			relationObject =(Relation) getHibernateObject(Relation.class, "id", getRelation());
		}
		
		return relationObject;
	}

	public Ontology getOntologyObject() {
		if(ontologyObject == null && ontology != null){
			ontologyObject =(Ontology) getHibernateObject(Ontology.class, "id", this.ontology);
		}
		
		return ontologyObject;
	}
	
	public Relation getSuperRelationObject() {
		if(superRelationObject == null && superRelation != null){
			superRelationObject = (Relation) getHibernateObject(Relation.class, "id", superRelation);
		}
		
		return superRelationObject;
	}
	

}
