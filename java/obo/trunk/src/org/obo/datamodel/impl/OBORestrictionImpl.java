package org.obo.datamodel.impl;

import java.io.*;

import org.obo.datamodel.*;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.TermUtil;

/** This is a relationship between terms/classes - not instances
    (to relate instances use InstancePropertyValue) */
import org.apache.log4j.*;

public class OBORestrictionImpl implements OBORestriction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBORestrictionImpl.class);

	protected static class OBORestrictionMetaData implements Serializable {
		protected boolean necessarilyTrue = true;

		protected boolean inverseNecessarilyTrue = false;

		protected boolean completes = false;

		protected boolean inverseCompletes = false;

		protected Integer maxCardinality;

		protected Integer minCardinality;

		protected Integer cardinality;		

		protected Namespace namespace;
		
		public boolean isEmpty() {
			return necessarilyTrue &&
			!inverseNecessarilyTrue &&
			!completes &&
			!inverseCompletes &&
			cardinality == null &&
			maxCardinality == null &&
			minCardinality == null &&
			namespace == null;
		}
	}
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -5348272558004398963L;

	protected transient LinkedObject child;

	protected transient LinkedObject parent;

	protected transient OBOProperty type;

	protected NestedValue nv;

	protected boolean implied = false;
	
	protected OBORestrictionMetaData metadata = null;

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}
	
	protected void insureMetadataObj() {
		if (metadata == null)
			metadata = new OBORestrictionMetaData();
	}
	
	protected void tryMetadataCleanup() {
		if (metadata != null && metadata.isEmpty())
			metadata = null;
	}

	public void setCardinality(Integer cardinality) {
		insureMetadataObj();
		metadata.cardinality = cardinality;
		tryMetadataCleanup();
	}

	public void setMaxCardinality(Integer maxCardinality) {
		insureMetadataObj();
		metadata.maxCardinality = maxCardinality;
		tryMetadataCleanup();
	}

	public void setMinCardinality(Integer minCardinality) {
		insureMetadataObj();
		metadata.minCardinality = minCardinality;
		tryMetadataCleanup();
	}
	
	public void setCompletes(boolean completes) {
		insureMetadataObj();
		metadata.completes = completes;
		tryMetadataCleanup();
	}

	public void setInverseCompletes(boolean inverseCompletes) {
		insureMetadataObj();
		metadata.inverseCompletes = inverseCompletes;
		tryMetadataCleanup();
	}

	public void setNamespace(Namespace namespace) {
		insureMetadataObj();
		metadata.namespace = namespace;
		tryMetadataCleanup();
	}

	public void setNecessarilyTrue(boolean necessarilyTrue) {
		insureMetadataObj();
		metadata.necessarilyTrue = necessarilyTrue;
		tryMetadataCleanup();
	}

	public void setInverseNecessarilyTrue(boolean inverseNecessarilyTrue) {
		insureMetadataObj();
		metadata.inverseNecessarilyTrue = inverseNecessarilyTrue;
		tryMetadataCleanup();
	}

	public Integer getCardinality() {
		return (metadata == null ? null : metadata.cardinality);
	}

	public Integer getMaxCardinality() {
		return (metadata == null ? null : metadata.maxCardinality);
	}

	public Integer getMinCardinality() {
		return (metadata == null ? null : metadata.minCardinality);
	}

	public boolean completes() {
		return (metadata == null ? false : metadata.completes);
	}

	public boolean inverseCompletes() {
		return (metadata == null ? false : metadata.inverseCompletes);
	}

	public void setNestedValue(NestedValue nv) {
		this.nv = nv;
	}

	public NestedValue getNestedValue() {
		return nv;
	}

	public boolean isNecessarilyTrue() {
		return (metadata == null ? true : metadata.necessarilyTrue);
	}

	public boolean isInverseNecessarilyTrue() {
		return (metadata == null ? false : metadata.inverseNecessarilyTrue);
	}

	public Namespace getNamespace() {
		return (metadata == null ? null : metadata.namespace);
	}

	public OBORestrictionImpl(LinkedObject child) {
		this(child, (OBOProperty) null, (LinkedObject) null);
	}

	public OBORestrictionImpl(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		this(child, type, parent, false);
	}

	public OBORestrictionImpl(LinkedObject child, OBOProperty type,
			LinkedObject parent, boolean implied) {
		this.child = child;
		this.parent = parent;
		this.type = type;
		this.implied = implied;
	}

	public boolean isImplied() {
		return implied;
	}
	
	public OBORestrictionImpl(boolean implied) {
		this(null, null, null, implied);
	}

	public OBORestrictionImpl(Link link) {
		this(link.getChild(), link.getType(), link.getParent());
	}

	public OBORestrictionImpl(Link link, boolean completes) {
		this(link);
		setCompletes(completes);
	}

	public OBORestrictionImpl(LinkedObject child, LinkedObject parent,
			OBOProperty type) {
		this(child, type, parent);
	}

	public OBORestrictionImpl() {
	}

	public LinkedObject getChild() {
		return child;
	}

	public LinkedObject getParent() {
		return parent;
	}

	public OBOProperty getType() {
		return type;
	}

	public void setType(OBOProperty in) {
		type = in;
	}

	public void setChild(LinkedObject child) {
		this.child = child;
		if (ForwardChainingReasoner.checkRecache) {
			if (child.equals(ForwardChainingReasoner.weirdLink.getChild()))
				logger.info("what the hell");
		}
	}

	public void setParent(LinkedObject parent) {
		this.parent = parent;
	}

	@Override
	public int hashCode() {

		int childHash = 0;
		int parentHash = 0;
		int typeHash = (type == null ? 0 : type.hashCode());

		if (child != null)
			childHash = child.hashCode();

		if (parent != null)
			parentHash = parent.hashCode();

		int hashCode = childHash + parentHash + typeHash;

		if (completes())
			hashCode *= 2;
		return hashCode;
	}

	@Override
	public boolean equals(Object o) {
		if (o == null) {
			return false;
		} else if (o instanceof Link) {
			return TermUtil.equals(this, (Link) o);
		} else
			return false;
	}

	@Override
	public String toString() {
		String bar = "--";
		if (completes())
			bar = "~~";
		else if (isImplied())
			bar = "==";
		return getChild() + " " + bar + (type == null ? "null" : type.getID())
				+ bar + "> " + getParent();
	}

	private void writeObject(java.io.ObjectOutputStream stream)
			throws IOException {
		stream.writeObject(child.getID());
		stream.writeObject(type.getID());
		stream.writeObject(parent.getID());
		stream.defaultWriteObject();
	}
	
	private void readObject(java.io.ObjectInputStream stream) throws IOException,
	ClassNotFoundException {
		String childID = (String) stream.readObject();
		String typeID = (String) stream.readObject();
		String parentID = (String) stream.readObject();
		if (OBOSessionImpl.serializingSession == null) {
			throw new IllegalStateException("Could not obtain link deserialization context");
		}
		
		child = (LinkedObject) OBOSessionImpl.serializingSession.getObject(childID);
		type = (OBOProperty) OBOSessionImpl.serializingSession.getObject(typeID);
		parent = (LinkedObject) OBOSessionImpl.serializingSession.getObject(parentID);
	}

	public String getID() {
		char sepChar = '-';
		if (completes())
			sepChar = '~';
		String childID = "null";
		String typeID = "null";
		String parentID = "null";
		if (getChild() != null)
			childID = getChild().getID();
		if (getType() != null)
			typeID = getType().getID();
		if (getParent() != null)
			parentID = getParent().getID();
		return childID + sepChar + typeID + sepChar + '>' + parentID;
	}
	
	
	public boolean isAnonymous() {
		return false;
	}
}
