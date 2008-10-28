package org.geneontology.db.model;

/**
 * The CVTermRelationship class corresponds to the GO term2term
 * table. CVTermRelationship relates one {@link CVTerm} object to another.
 * 
 * @author Suzanna Lewis
 * 
 */
public class MetaRelationship extends AbstractRelationship {
	

	public MetaRelationship(){
		String[] uniqueConstraintFields = {"type","subject", "object"};
		this.initUniqueConstraintFields(MetaRelationship.class,uniqueConstraintFields);
	}
	
	
}