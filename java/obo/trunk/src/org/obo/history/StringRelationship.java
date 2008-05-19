/**
 * 
 */
package org.obo.history;

import java.io.Serializable;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;

import org.apache.log4j.*;

public class StringRelationship implements Serializable, Cloneable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(StringRelationship.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected String parent;

	protected String child;

	protected String type;

	protected boolean completes;

	protected boolean necessary = true;

	protected boolean inverse_necessary;

	protected Integer cardinality;

	protected Integer minCardinality;

	protected Integer maxCardinality;

	protected String ns;

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			return null;
		}
	}

	public String getNamespace() {
		return ns;
	}

	public void setCompletes(boolean completes) {
		this.completes = completes;
	}

	public boolean completes() {
		return completes;
	}

	public boolean isNecessary() {
		return necessary;
	}

	public boolean isInverseNecessary() {
		return inverse_necessary;
	}

	public Integer getCardinality() {
		return cardinality;
	}

	public Integer getMinCardinality() {
		return minCardinality;
	}

	public Integer getMaxCardinality() {
		return maxCardinality;
	}

	public StringRelationship() {
	}

	public StringRelationship(OBORestriction rel) {
		this(rel.getChild(), rel.getType(), rel.getParent(), rel
				.completes(), rel.isNecessarilyTrue(), rel
				.isInverseNecessarilyTrue(), rel.getCardinality(), rel
				.getMinCardinality(), rel.getMaxCardinality(),
				(rel.getNamespace() == null ? null : rel.getNamespace()
						.getID()));
	}

	public StringRelationship(Link rel) {
		this(rel.getChild(), rel.getType(), rel.getParent(), false, false,
				false, null, null, null, (rel.getNamespace() == null ? null
						: rel.getNamespace().getID()));
	}

	public StringRelationship(LinkedObject childTerm,
			OBOProperty typeTerm, LinkedObject parentTerm) {
		this(childTerm, typeTerm, parentTerm, false, true, false, null,
				null, null, null);
	}

	public StringRelationship(LinkedObject childTerm,
			OBOProperty typeTerm, LinkedObject parentTerm,
			boolean completes, boolean necessary,
			boolean inverse_necessary, Integer cardinality,
			Integer minCardinality, Integer maxCardinality, String ns) {
		this(childTerm == null ? null : childTerm.getID(),
				typeTerm == null ? null : typeTerm.getID(),
				parentTerm == null ? null : parentTerm.getID(), completes,
				necessary, inverse_necessary, cardinality, minCardinality,
				maxCardinality, ns);
	}

	public StringRelationship(String child, String type, String parent) {
		this(child, type, parent, false, true, false, null, null, null,
				null);
	}

	public StringRelationship(String child, String type, String parent,
			boolean completes) {
		this(child, type, parent, completes, false, false, null, null,
				null, null);
	}

	public StringRelationship(String child, String type, String parent,
			boolean completes, boolean necessary,
			boolean inverse_necessary, Integer cardinality,
			Integer minCardinality, Integer maxCardinality, String ns) {
		this.parent = parent;
		this.child = child;
		this.type = type;
		this.completes = completes;
		this.necessary = necessary;
		this.inverse_necessary = inverse_necessary;
		this.cardinality = cardinality;
		this.minCardinality = minCardinality;
		this.maxCardinality = maxCardinality;
		this.ns = ns;
	}

	@Override
	public boolean equals(Object o) {
		StringRelationship sr = (StringRelationship) o;
		return ObjectUtil.equals(parent, sr.getParent())
				&& ObjectUtil.equals(child, sr.getChild())
				&& ObjectUtil.equals(type, sr.getType());
	}

	@Override
	public int hashCode() {
		return (parent == null ? 0 : parent.hashCode())
				^ (child == null ? 0 : child.hashCode())
				^ (type == null ? 0 : type.hashCode());
	}

	public void setParent(String parent) {
		this.parent = parent;
	}

	public void setChild(String child) {
		this.child = child;
	}

	public void setType(String type) {
		this.type = type;
	}

	public StringRelationship(String parent) {
		this(parent, null, null, false, false, false, null, null, null,
				null);
	}

	public String getParent() {
		return parent;
	}

	public String getChild() {
		return child;
	}

	public String getType() {
		return type;
	}

	@Override
	public String toString() {
		return child + " --" + type + "--> " + parent;
	}

	public boolean canForward(String oldID) {
		boolean forwarded = false;
		if (parent != null && parent.equals(oldID)) {
			forwarded = true;
		}
		if (child != null && child.equals(oldID)) {
			forwarded = true;
		}
		if (type != null && type.equals(oldID)) {
			forwarded = true;
		}
		return forwarded;
	}

	public boolean forwardID(String oldID, String newID) {
		boolean forwarded = false;
		if (parent != null && parent.equals(oldID)) {
			setParent(newID);
			forwarded = true;
		}
		if (child != null && child.equals(oldID)) {
			setChild(newID);
			forwarded = true;
		}
		if (type != null && type.equals(oldID)) {
			forwarded = true;
			setType(newID);
		}
		return forwarded;
	}
}
