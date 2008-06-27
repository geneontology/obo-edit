package org.obo.datamodel.impl;

import org.bbop.util.StringUtil;
import org.bbop.util.TinySet;
import org.obo.datamodel.*;
import org.obo.postcomp.OBOPostcomp;
import org.apache.log4j.*;

import java.util.*;

public abstract class AnnotatedObjectImpl implements AnnotatedObject,
		CategorizedObject {

//	initialize logger
	protected final static Logger logger = Logger.getLogger(AnnotatedObjectImpl.class);
	
	protected static int internal_id_gen = 0;

	public int internal_id = internal_id_gen++;

	protected String id;
	
	protected int hashCode;

	protected boolean anonymous = false;

	protected boolean isObsolete = false;

	// protected ObsoletableObject replacedBy;
	protected Set considerReplacements = new TinySet();

	protected Set replacements = new TinySet();

	protected Set categories = new TinySet();

	protected Set synonyms = new TinySet();

	protected Set dbxrefs = new TinySet();

	protected Set<String> secondaryIDs = new TinySet<String>();

	protected Set<Dbxref> defReferences = new TinySet<Dbxref>();

	protected Set propertyValues = new TinySet();

	protected HashMap categoryExtensionHash; // = new HashMap();

	protected NestedValue definitionExtension;

	protected String definition = "";

	protected Namespace namespace;

	protected String name;

	protected String comment = "";

	protected NestedValue idExtension;

	protected NestedValue nameExtension;

	protected NestedValue namespaceExtension;

	protected NestedValue commentExtension;

	protected NestedValue obsoleteExtension;

	protected NestedValue anonymousExtension;

	protected NestedValue typeExtension;

	protected HashMap secondaryIDExtensionMap;

	protected HashMap considerExtensionMap;

	protected HashMap replacedByExtensionMap;
	
	protected String createdBy;
	
	protected Date creationDate;
	
	protected String modifiedBy;
	
	protected Date modificationDate;
	
	protected NestedValue modificationDateExtension;
	protected NestedValue createdByExtension;
	protected NestedValue modifiedByExtension;
	protected NestedValue creationDateExtension;

	public AnnotatedObjectImpl(String id) {
		this.id = id;
		// store the hashcode independently; this way if we change the ids we don't
		// have to update any hash tables; there will probably be more hashtable
		// collisions, but it doesn't matter. We shouldn't be changing ids frequently anyway
		hashCode = id.hashCode();
	}
	
	public String getID() {
		return id;
	}

	public boolean isAnonymous() {
		return anonymous;
	}

	public void setIsAnonymous(boolean isAnonymous) {
		this.anonymous = isAnonymous;
	}

	public boolean isBuiltIn() {
		return false;
	}

	public boolean isObsolete() {
		return isObsolete;
	}

	public void setObsolete(boolean isObsolete) {
		this.isObsolete = isObsolete;
	}

	public Set getReplacedBy() {
		return replacements;
	}

	public void addReplacedBy(ObsoletableObject replacedBy) {
		replacements.add(replacedBy);
	}

	public void removeReplacedBy(ObsoletableObject replacedBy) {
		replacements.remove(replacedBy);
	}

	public Set getConsiderReplacements() {
		return considerReplacements;
	}

	public void addConsiderReplacement(ObsoletableObject o) {
		considerReplacements.add(o);
	}

	public void removeConsiderReplacement(ObsoletableObject o) {
		considerReplacements.remove(o);
	}

	public Set getSynonyms() {
		return synonyms;
	}

	public void addSynonym(Synonym s) {
		synonyms.add(s);
	}

	public void removeSynonym(Synonym s) {
		synonyms.remove(s);
	}

	public Set getDbxrefs() {
		return dbxrefs;
	}

	public void addDbxref(Dbxref xref) {
		dbxrefs.add(xref);
	}

	public void removeDbxref(Dbxref xref) {
		// we iterate through the dbxrefs because otherwise the hashtable
		// won't correctly delete unmodified dbxrefs
		Iterator it = dbxrefs.iterator();
		while (it.hasNext()) {
			Dbxref ref = (Dbxref) it.next();
			if (ref.equals(xref)) {
				logger.info("removing matching dbxref " + ref);
				it.remove();
				break;
			}
		}
	}

	public void addSecondaryID(String secondaryID) {
		secondaryIDs.add(secondaryID);
	}

	public void removeSecondaryID(String secondaryID) {
		secondaryIDs.remove(secondaryID);
	}

	public Set getSecondaryIDs() {
		return secondaryIDs;
	}

	public Namespace getNamespace() {
		return namespace;
	}

	public String getName() {
		return name;
	}

	public String getComment() {
		return comment;
	}

	public void setNamespace(Namespace namespace) {
		this.namespace = namespace;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public void setIDExtension(NestedValue nv) {
		this.idExtension = nv;
	}

	public void setNameExtension(NestedValue nv) {
		this.nameExtension = nv;
	}

	public void setCommentExtension(NestedValue nv) {
		this.commentExtension = nv;
	}

	public void setObsoleteExtension(NestedValue nv) {
		this.obsoleteExtension = nv;
	}

	public void setAnonymousExtension(NestedValue nv) {
		this.anonymousExtension = nv;
	}

	public void setNamespaceExtension(NestedValue nv) {
		this.namespaceExtension = nv;
	}

	public void setTypeExtension(NestedValue nv) {
		this.typeExtension = nv;
	}

	public void addReplacedByExtension(ObsoletableObject oo, NestedValue nv) {
		if (replacedByExtensionMap == null)
			replacedByExtensionMap = new HashMap();
		replacedByExtensionMap.put(oo, nv);
	}

	public NestedValue getReplacedByExtension(ObsoletableObject oo) {
		if (replacedByExtensionMap == null)
			return null;
		else
			return (NestedValue) replacedByExtensionMap.get(oo);
	}

	public void addSecondaryIDExtension(String id, NestedValue nv) {
		if (secondaryIDExtensionMap == null)
			secondaryIDExtensionMap = new HashMap();
		secondaryIDExtensionMap.put(id, nv);
	}

	public NestedValue getSecondaryIDExtension(String id) {
		if (secondaryIDExtensionMap == null)
			return null;
		else
			return (NestedValue) secondaryIDExtensionMap.get(id);
	}

	public void addConsiderExtension(ObsoletableObject o, NestedValue nv) {
		if (considerExtensionMap == null)
			considerExtensionMap = new HashMap();
		considerExtensionMap.put(o, nv);
	}

	public NestedValue getConsiderExtension(ObsoletableObject o) {
		if (considerExtensionMap == null)
			return null;
		else
			return (NestedValue) considerExtensionMap.get(o);

	}

	public NestedValue getIDExtension() {
		return idExtension;
	}

	public NestedValue getNameExtension() {
		return nameExtension;
	}

	public NestedValue getCommentExtension() {
		return commentExtension;
	}

	public NestedValue getObsoleteExtension() {
		return obsoleteExtension;
	}

	public NestedValue getAnonymousExtension() {
		return anonymousExtension;
	}

	public NestedValue getNamespaceExtension() {
		return namespaceExtension;
	}

	public NestedValue getTypeExtension() {
		return typeExtension;
	}

	public void addPropertyValue(PropertyValue pv) {
		if (propertyValues == null)
			propertyValues = new TinySet();
		propertyValues.add(pv);
	}

	public Set<Dbxref> getDefDbxrefs() {
		return defReferences;
	}

	public void addDefDbxref(Dbxref ref) {
		if (ref.getType() != Dbxref.DEFINITION)
			(new Exception("adding non-definition dbxref " + ref
					+ " as dbxref.")).printStackTrace();
		defReferences.add(ref);
	}

	public void removeDefDbxref(Dbxref ref) {
		defReferences.remove(ref);
	}

	public String getDefinition() {
		return definition;
	}

	public void setDefinition(String definition) {
		this.definition = definition;
	}

	public void addCategoryExtension(TermCategory category, NestedValue nv) {
		if (categoryExtensionHash == null)
			categoryExtensionHash = new HashMap();
		categoryExtensionHash.put(category, nv);
	}

	public NestedValue getCategoryExtension(TermCategory category) {
		if (categoryExtensionHash == null)
			return null;
		return (NestedValue) categoryExtensionHash.get(category);
	}

	public void setDefinitionExtension(NestedValue definitionExtension) {
		this.definitionExtension = definitionExtension;
	}

	public NestedValue getDefinitionExtension() {
		return definitionExtension;
	}

	public Set getPropertyValues() {
		return propertyValues;
	}

	public void removePropertyValue(PropertyValue pv) {
		if (propertyValues != null)
			propertyValues.remove(pv);
	}

	public void addCategory(TermCategory category) {
		categories.add(category);
	}

	public void removeCategory(TermCategory category) {
		categories.remove(category);
	}

	public Set getCategories() {
		return categories;
	}

	@Override
	public String toString() {
		return getName() + " (" + getID() + ")";
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof IdentifiableObject) {
			IdentifiableObject io = (IdentifiableObject) o;
			return StringUtil.requals(io.getID(), getID());
		} else
			return false;
	}

	@Override
	public Object clone() {
		try {
			AnnotatedObjectImpl out = (AnnotatedObjectImpl) super.clone();
			out.internal_id = internal_id_gen++;

			Iterator it;

			out.synonyms = new TinySet();
			it = synonyms.iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				out.addSynonym((Synonym) s.clone());
			}

			out.dbxrefs = new TinySet();
			it = getDbxrefs().iterator();
			while (it.hasNext()) {
				Dbxref d = (Dbxref) it.next();
				out.addDbxref((Dbxref) d.clone());
			}

			out.defReferences = new TinySet();
			it = getDefDbxrefs().iterator();
			while (it.hasNext()) {
				Dbxref d = (Dbxref) it.next();
				out.addDefDbxref((Dbxref) d.clone());
			}

			out.categories = new TinySet();
			it = categories.iterator();
			while (it.hasNext()) {
				out.addCategory((TermCategory) it.next());
			}

			out.secondaryIDs = new TinySet();
			it = secondaryIDs.iterator();
			while (it.hasNext()) {
				out.addSecondaryID((String) it.next());
			}

			return out;
		} catch (Exception ex) {
			return null;
		}
	}

	@Override
	public int hashCode() {
		return hashCode;
	}

	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	public Date getCreationDate() {
		return creationDate;
	}

	public void setCreationDate(Date creationDate) {
		this.creationDate = creationDate;
	}

	public Date getModificationDate() {
		return modificationDate;
	}

	public void setModificationDate(Date modificationDate) {
		this.modificationDate = modificationDate;
	}

	public String getModifiedBy() {
		return modifiedBy;
	}

	public void setModifiedBy(String modifiedBy) {
		this.modifiedBy = modifiedBy;
	}

	public NestedValue getCreationDateExtension() {
		return creationDateExtension;
	}

	public void setCreationDateExtension(NestedValue creationDateExtension) {
		this.creationDateExtension = creationDateExtension;
	}

	public NestedValue getModificationDateExtension() {
		return modificationDateExtension;
	}

	public void setModificationDateExtension(NestedValue modificationDateExtension) {
		this.modificationDateExtension = modificationDateExtension;
	}

	public NestedValue getModifiedByExtension() {
		return modifiedByExtension;
	}

	public void setModifiedByExtension(NestedValue modifiedByExtension) {
		this.modifiedByExtension = modifiedByExtension;
	}

	public NestedValue getCreatedByExtension() {
		return createdByExtension;
	}

	public void setCreatedByExtension(NestedValue createdByExtension) {
		this.createdByExtension = createdByExtension;
	}
}
