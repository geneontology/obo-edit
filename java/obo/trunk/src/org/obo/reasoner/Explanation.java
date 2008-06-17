package org.obo.reasoner;

import java.io.Serializable;
import java.util.*;

import org.obo.datamodel.Link;
import org.obo.datamodel.PathCapable;

public interface Explanation extends Cloneable, Serializable {

	public ExplanationType getExplanationType();

	public Collection<Link> getEvidence();
	
	public void addEvidence(Link link);
	
	/**
	 * @param link
	 * @return true if removing the link invalidated the explanation
	 */
	public boolean removeEvidence(Link link);
	
	public void setDesc(String desc);
	
	/**
	 * note that despite the name of this method, it does not return an "object" in the sense of a LinkedObject etc;
	 * it returns a PathCapable object, i.e. a Link
	 * @return
	 */
	public PathCapable getExplainedObject();

	public String getDesc();
}
