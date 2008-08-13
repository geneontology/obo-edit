package org.geneontology.db.model;

import org.gmod.db.chAPI.simpleObject.CVTerm;
import org.gmod.db.chAPI.simpleObject.CVTermRelationship;

/**
 * The CVTermRelationship class corresponds to the GO term2term
 * table. CVTermRelationship relates one {@link CVTerm} object to another.
 * 
 * @author Suzanna Lewis
 * 
 */
public class GOTermRelationship extends CVTermRelationship {
	
	/** The set of all edges with complete=1 for any term2_id states the NECESSARY AND SUFFICIENT CONDITIONS for that term. */
	protected int complete;

	public GOTermRelationship(){
		super();
	}
	
	/**
	 * Getter of CVTerm complete flag.
	 * @return 1 if root, 0 otherwise.
	 */
	public Integer getComplete() {
		return complete;
	}
	
	/**
	 * Setter of CVTerm is complete flag.
	 * @param complete flag.
	 */
	public void setComplete(Integer complete) {
		this.complete = complete;
	}

}