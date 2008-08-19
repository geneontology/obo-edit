package org.geneontology.db.model;

/**
 * The CVTermRelationship class corresponds to the GO term2term
 * table. CVTermRelationship relates one {@link CVTerm} object to another.
 * 
 * @author Suzanna Lewis
 * 
 */
public class MetaRelation extends GOModel {
	
	/** The cvterm_relationship_id of the CVTermRelationship} */
	public int relation_id;
	
	/** The {@link CVTerm} type of this relationship */
	public Term type;
	
	/** The subject {@link CVTerm} term */
	public Term subject;
	
	/** The object {@link CVTerm} term */
	public Term object;

	public MetaRelation(){
		String[] uniqueConstraintFields = {"subject", "object"};
		this.initUniqueConstraintFields(MetaRelation.class,uniqueConstraintFields);
	}
	
	/**
	 * Getter of CVTermRelationship cvterm_relationship_id.
	 * @return the cvterm_relationship_id of the CVTermRelationship.
	 */
	public int getRelation_id() {
		return relation_id;
	}
	
	/**
	 * Setter of CVTermRelationship cvterm_relationship_id
	 * @param cvterm_relationship_id the cvterm_relationship_id of this CVTermRelationship.
	 */
	public void setRelation_id(int relation_id) {
		this.relation_id = relation_id;
	}
	
	/**
	 * Getter of CVTermRelationship relationship type.
	 * @return CVTermRelationship relationship type.
	 */
	public Term getType() {
		return type;
	}
	
	/**
	 * Setter of CVTermRelationship relationship type.
	 * @param type CVTermRelationship relationship type.
	 */
	public void setType(Term type) {
		this.type = type;
	}

	/**
	 * Getter of CVTermRelationship subject {@link CVTerm}.
	 * @return CVTermRelationship subject {@link CVTerm}.
	 */
	public Term getSubject() {
		return subject;
	}
	
	/**
	 * Setter of CVTermRelationship subject {@link CVTerm}.
	 * @param subject CVTermRelationship subject {@link CVTerm}.
	 */
	public void setSubject(Term subject) {
		this.subject = subject;
	}
	
	/**
	 * Getter of CVTermRelationship object {@link CVTerm}.
	 * @return the CVTermRelationship object {@link CVTerm}.
	 */
	public Term getObject() {
		return object;
	}
	
	/**
	 * Setter of CVTermRelationship object {@link CVTerm}.
	 * @param object CVTermRelationship object {@link CVTerm}.
	 */
	public void setObject(Term object) {
		this.object = object;
	}
	
}