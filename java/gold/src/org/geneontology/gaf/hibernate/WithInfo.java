package org.geneontology.gaf.hibernate;

import java.io.Serializable;

/**
 * The class represents the with_info table in the database. Please see WithInfo.hbm.xml file in this 
 * for the mapping detail 
 * @author Shahid Manzoor
 *
 */

public class WithInfo extends owltools.gaf.WithInfo implements Serializable {

	public WithInfo() {
		super();
	}

	public WithInfo(String id, String withXref) {
		super(id, withXref);
	}

	/**
	 * This method generates unique hashcode used by hibernate.
	 */
	public int hashCode() {
		int result = 17;
		
		result = 37 * result + (getId() == null ? 0 : getId().hashCode());
		result = 37 * result + (getWithXref() == null ? 0 : getWithXref().hashCode());
		
		return result;
	}

	public boolean equals(Object other) {
		if(this == other)
			return true;
		
		if(other == null)
			return false;
	
		if(!(other instanceof WithInfo))
			return false;
		
		
		WithInfo wi = (WithInfo) other;
		
		boolean result = (getId() == wi.getId() || getId() != null && getId().equals(wi.getId()) )
			&& (getWithXref() == wi.getWithXref() || getWithXref() != null && getWithXref().equals(wi.getWithXref()))
		
			;
		
		return result;
	
	}
	
	
}
