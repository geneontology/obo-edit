package org.geneontology.refgenome.client;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedList;

/**
 * @author cjm
 *
 */
public class StatementDTO extends NodeDTO implements Serializable {
	protected String nodeId;
	protected String relationId;
	protected String targetId;
	protected String positedByNodeId;
	protected String contextId;
	protected boolean isInferred;
	protected Collection<StatementDTO> subStatements;
	protected boolean appliesToAllInstancesOf;
	protected boolean intersectionSemantics;
	protected boolean unionSemantics;
//	protected InstanceQuantifier instanceQuantifier;
	
	public StatementDTO() {
		super();
	}
	public StatementDTO(String id) {
		super();
		nodeId = id;
	}
	public StatementDTO(String n, String r, String t) {
		super();
		nodeId = n;
		relationId = r;
		targetId = t;
	}

	public boolean isAppliesToAllInstancesOf() {
		return appliesToAllInstancesOf;
	}
	public void setAppliesToAllInstancesOf(boolean appliesToAllInstancesOf) {
		this.appliesToAllInstancesOf = appliesToAllInstancesOf;
	}
	
	/*
	public boolean isExistential() {
		if (instanceQuantifier == null)
			return false;
		else
			return instanceQuantifier.isExistential();
	}
	public void setExistential(boolean isExistential) {
		if (instanceQuantifier == null)
			instanceQuantifier = new InstanceQuantifier();
		 instanceQuantifier.setExistential(isExistential);
	}
	public boolean isUniversal() {
		if (instanceQuantifier == null)
			return false;
		else
			return instanceQuantifier.isUniversal();
	}
	public void setUniversal(boolean isUniversal) {
		if (instanceQuantifier == null)
			instanceQuantifier = new InstanceQuantifier();
		 instanceQuantifier.setUniversal(isUniversal);
	}
	public boolean isIntersectionSemantics() {
		return intersectionSemantics;
	}
	public void setIntersectionSemantics(boolean hasIntersectionSemantics) {
		this.intersectionSemantics = hasIntersectionSemantics;
	}
	public boolean isUnionSemantics() {
		return unionSemantics;
	}
	public void setUnionSemantics(boolean hasUnionSemantics) {
		this.unionSemantics = hasUnionSemantics;
	}
	public boolean isInferred() {
		return isInferred;
	}
	public void setInferred(boolean isImplied) {
		this.isInferred = isImplied;
	}
	public InstanceQuantifier getInstanceQuantifier() {
		return instanceQuantifier;
	}
	public void setInstanceQuantifier(InstanceQuantifier instanceQuantifier) {
		this.instanceQuantifier = instanceQuantifier;
	}
*/
	
	public String getPositedByNodeId() {
		return positedByNodeId;
	}
	public void setPositedByNodeId(String positedByNodeId) {
		this.positedByNodeId = positedByNodeId;
	}
	/**
	 * @return the identifier of the node of which this statement is about
	 */
	public String getNodeId() {
		return nodeId;
	}
	public void setNodeId(String nodeId) {
		this.nodeId = nodeId;
	}
	/**
	 * @return the identifier of the relation node used in this statement
	 */
	public String getRelationId() {
		return relationId;
	}
	public void setRelationId(String relationId) {
		this.relationId = relationId;
	}
	/**
	 * @return the identifier of the relatum node
	 */
	public String getTargetId() {
		return targetId;
	}
	public void setTargetId(String targetId) {
		this.targetId = targetId;
	}
	
	public String getContextId() {
		return contextId;
	}
	public void setContextId(String contextId) {
		this.contextId = contextId;
	}
	
	
	public Collection<StatementDTO> getSubStatements() {
		if (subStatements == null)
			subStatements = new LinkedList<StatementDTO>();
		return subStatements;
	}
	public void setSubStatements(Collection<StatementDTO> subStatements) {
		this.subStatements = subStatements;
	}
	public void addSubStatement(StatementDTO s) {
		if (subStatements == null)
			subStatements = new LinkedList<StatementDTO>();
		subStatements.add(s);
	}
	
	public String toString() {
		String s = nodeId+" "+relationId+" "+targetId;
		if (isInferred)
			s = s + " [Implied]";
		if (positedByNodeId != null)
			s = s + " positedBy:"+positedByNodeId;
		if (sourceId != null)
			s = s + " source:"+sourceId;
		for (StatementDTO ss : getSubStatements()) {
			s = s + " <<"+ss.toString()+">> ";
		}
		return s;
	}
	
	public int hashCode() {
		return toString().hashCode();
	}

}
