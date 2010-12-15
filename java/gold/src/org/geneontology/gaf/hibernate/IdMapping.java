package org.geneontology.gaf.hibernate;

import java.io.Serializable;

import org.geneontology.gold.hibernate.model.GOModel;

public class IdMapping extends GOModel implements Serializable {

	private String sourceId;
	private String targetId;
	private String relationship;
	private String mappingSource;
	
	public IdMapping(){
		String keys[] = {"sourceId", "targetId", "relationship", "mappingSource"};
		this.initUniqueConstraintFields(IdMapping.class, keys);
	}

	public IdMapping(String sourceId, String targetId, String relationship,
			String mappingSource) {
		this();
		this.sourceId = sourceId;
		this.targetId = targetId;
		this.relationship = relationship;
		this.mappingSource = mappingSource;
	}

	public String getSourceId() {
		return sourceId;
	}

	public void setSourceId(String sourceId) {
		this.sourceId = sourceId;
	}

	public String getTargetId() {
		return targetId;
	}

	public void setTargetId(String targetId) {
		this.targetId = targetId;
	}

	public String getRelationship() {
		return relationship;
	}

	public void setRelationship(String relationship) {
		this.relationship = relationship;
	}

	public String getMappingSource() {
		return mappingSource;
	}

	public void setMappingSource(String mappingSource) {
		this.mappingSource = mappingSource;
	}
	
	
	
	
	
	
}
