package org.geneontology.gaf.hibernate;

import java.io.Serializable;

public class IdMapping extends owltools.gaf.IdMapping implements Serializable {

	public IdMapping() {
		super();
	}

	public IdMapping(String sourceId, String targetId, String relationship,
			String mappingSource) {
		super(sourceId, targetId, relationship, mappingSource);
	}

	public int hashCode() {
		int result = 17;
	
		result = 37 * result + (getSourceId() == null ? 0 : getSourceId().hashCode());
		result = 37 * result + (getTargetId() == null ? 0 : getTargetId().hashCode());
		result = 37 * result + (getRelationship() == null ? 0 : getRelationship().hashCode());
		result = 37 * result + (getMappingSource() == null ? 0 : getMappingSource().hashCode());
		
		return result;
	}

	public boolean equals(Object other) {
		if(this == other)
			return true;
		
		if(other == null)
			return false;
	
		if(!(other instanceof IdMapping))
			return false;
		
		IdMapping im = (IdMapping) other;
		
		boolean result =( getSourceId() == im.getSourceId() || getSourceId() != null && getSourceId().equals(im.getSourceId()) )
			&& (getTargetId() == im.getTargetId() || getTargetId() != null && getTargetId().equals(im.getTargetId()))
			&& (getRelationship() == im.getRelationship() || getRelationship() != null && getRelationship().equals(im.getRelationship()) ) 
			&& (getMappingSource() == im.getMappingSource() || getMappingSource() != null && getMappingSource().equals(im.getMappingSource()))
			;
		
		return result;
	}	
	
}
