package org.obo.reasoner;

import java.io.Serializable;
import java.util.*;

import org.obo.datamodel.Link;
import org.obo.datamodel.PathCapable;

public interface Explanation extends Cloneable, Serializable {

	public ExplanationType getExplanationType();

	public Collection<Link> getEvidence();
	
	public void addEvidence(Link link);
	
	public boolean removeEvidence(Link link);
	
	public void setDesc(String desc);
	
	public PathCapable getExplainedObject();

	public String getDesc();
}
