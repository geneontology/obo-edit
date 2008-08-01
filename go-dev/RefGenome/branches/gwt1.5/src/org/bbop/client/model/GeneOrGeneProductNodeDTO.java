package org.bbop.client.model;

import java.util.Iterator;
import java.util.List;

public class GeneOrGeneProductNodeDTO extends NodeDTO {

	private NodeDTO inOrganism;
	
	public GeneOrGeneProductNodeDTO(NodeDTO n) {
		super(n.getId(),n.getLabel(),n.getSourceId());
		//initializeInOrganism();
	}

	/*
	private void initializeInOrganism() {
		List ids = getTargetIds("OBO_REL:in_organism");
		Iterator it = ids.iterator();
		if (it.hasNext())
			setIn
	}
	*/

	public NodeDTO getInOrganism() {
		return inOrganism;
	}

	public void setInOrganism(NodeDTO inOrganism) {
		this.inOrganism = inOrganism;
	}
	
	

	
	
}
