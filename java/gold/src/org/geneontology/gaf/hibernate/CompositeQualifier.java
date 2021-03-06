package org.geneontology.gaf.hibernate;

import java.io.Serializable;

/**
 * The class represents the composite_qualifier table in the database. Please see CompositeQualifier.hbm.xml file in this 
 * for the mapping detail 
 * @author Shahid Manzoor
 *
 */
public class CompositeQualifier extends owltools.gaf.CompositeQualifier implements Serializable {

	public CompositeQualifier() {
		super();
	}

	public CompositeQualifier(String id, String qualifierObj) {
		super(id, qualifierObj);
	}

	/**
	 * This method generates unique hashcode used by hibernate.
	 */
	public int hashCode() {
		int result = 37;
		
		result = 37 * result + (getId() == null ? 0 : getId().hashCode());
		result = 37 * result + (getQualifierObj() == null ? 0 : getQualifierObj().hashCode());
		
		return result;
	}

	public boolean equals(Object other) {
		if(this == other)
			return true;
		
		if(other == null)
			return false;
		
		if(!(other instanceof CompositeQualifier))
			return false;
		
		CompositeQualifier cq = (CompositeQualifier) other;
		
		boolean result = ( getId() == cq.getId() || ( getId() != null && getId().equals(cq.getId()) ) )
			&& (getQualifierObj() == cq.getQualifierObj() || (getQualifierObj() != null && getQualifierObj().equals(cq.getQualifierObj())))
			;
			
		return result;
	}
		
	
}
