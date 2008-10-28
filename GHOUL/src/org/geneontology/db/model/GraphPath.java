package org.geneontology.db.model;

/**
 * The CVTermRelationship class corresponds to the GO term2term
 * table. CVTermRelationship relates one {@link CVTerm} object to another.
 * 
 * @author Suzanna Lewis
 * 
 */
public class GraphPath extends AbstractRelationship {
	
	protected int distance;
	protected int relation_distance;
	 
	
	public GraphPath(){
		String[] uniqueConstraintFields = {"type", "subject", "object", "distance"};
		this.initUniqueConstraintFields(GraphPath.class,uniqueConstraintFields);
	}


	public int getDistance() {
		return distance;
	}


	public void setDistance(int distance) {
		this.distance = distance;
	}


	public int getRelation_distance() {
		return relation_distance;
	}


	public void setRelation_distance(int relation_distance) {
		this.relation_distance = relation_distance;
	}
	
	

}