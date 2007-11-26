package org.obo.datamodel.impl;

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.bbop.util.TinySet;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObjectFactory;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymCategory;
import org.obo.datamodel.TermCategory;
import org.obo.datamodel.Type;
import org.obo.datamodel.UnknownStanza;
import org.obo.history.HistoryItem;
import org.obo.history.OperationModel;
import org.obo.history.SessionHistoryList;
import org.obo.identifier.IDProfile;
import org.obo.query.QueryResolver;
import org.obo.query.impl.DefaultQueryResolver;

public class WriteCachedOBOSession implements OBOSession {

	protected class CacheWrappedOBOClass implements OBOClass {
		protected OBOClass oboClass;

		public CacheWrappedOBOClass(OBOClass oboClass) {
			this.oboClass = oboClass;
		}

		public void addCategory(TermCategory category) {
			oboClass.addCategory(category);
		}

		public void addCategoryExtension(TermCategory category, NestedValue nv) {
			oboClass.addCategoryExtension(category, nv);
		}

		public void addChild(Link tr) {
			oboClass.addChild(tr);
		}

		public void addConsiderExtension(ObsoletableObject o, NestedValue v) {
			oboClass.addConsiderExtension(o, v);
		}

		public void addConsiderReplacement(ObsoletableObject o) {
			oboClass.addConsiderReplacement(o);
		}

		public void addDbxref(Dbxref xref) {
			oboClass.addDbxref(xref);
		}

		public void addDefDbxref(Dbxref dbxref) {
			oboClass.addDefDbxref(dbxref);
		}

		public void addParent(Link tr) {
			oboClass.addParent(tr);
		}

		public void addPropertyValue(PropertyValue pv) {
			oboClass.addPropertyValue(pv);
		}

		public void addReplacedBy(ObsoletableObject o) {
			oboClass.addReplacedBy(o);
		}

		public void addReplacedByExtension(ObsoletableObject o, NestedValue v) {
			oboClass.addReplacedByExtension(o, v);
		}

		public void addSecondaryID(String secondaryID) {
			oboClass.addSecondaryID(secondaryID);
		}

		public void addSecondaryIDExtension(String id, NestedValue nv) {
			oboClass.addSecondaryIDExtension(id, nv);
		}

		public void addSynonym(Synonym s) {
			oboClass.addSynonym(s);
		}

		public void atomicAddChild(Link tr) {
			oboClass.atomicAddChild(tr);
		}

		public void atomicAddParent(Link tr) {
			oboClass.atomicAddParent(tr);
		}

		public void atomicRemoveChild(Link tr) {
			oboClass.atomicRemoveChild(tr);
		}

		public void atomicRemoveParent(Link tr) {
			oboClass.atomicRemoveParent(tr);
		}

		public Object clone() {
			return oboClass.clone();
		}

		public int compareTo(Object o) {
			return oboClass.compareTo(o);
		}

		public NestedValue getAnonymousExtension() {
			return oboClass.getAnonymousExtension();
		}

		public Set getCategories() {
			return oboClass.getCategories();
		}

		public NestedValue getCategoryExtension(TermCategory category) {
			return oboClass.getCategoryExtension(category);
		}

		public Collection<Link> getChildren() {
			return linkDatabase.getChildren(this);
		}

		public String getComment() {
			return oboClass.getComment();
		}

		public NestedValue getCommentExtension() {
			return oboClass.getCommentExtension();
		}

		public NestedValue getConsiderExtension(ObsoletableObject o) {
			return oboClass.getConsiderExtension(o);
		}

		public Set<ObsoletableObject> getConsiderReplacements() {
			return oboClass.getConsiderReplacements();
		}

		public Set<Dbxref> getDbxrefs() {
			return oboClass.getDbxrefs();
		}

		public Set<Dbxref> getDefDbxrefs() {
			return oboClass.getDefDbxrefs();
		}

		public String getDefinition() {
			return oboClass.getDefinition();
		}

		public NestedValue getDefinitionExtension() {
			return oboClass.getDefinitionExtension();
		}

		public String getID() {
			return oboClass.getID();
		}

		public NestedValue getIDExtension() {
			return oboClass.getIDExtension();
		}

		public String getName() {
			return oboClass.getName();
		}

		public NestedValue getNameExtension() {
			return oboClass.getNameExtension();
		}

		public Namespace getNamespace() {
			return oboClass.getNamespace();
		}

		public NestedValue getNamespaceExtension() {
			return oboClass.getNamespaceExtension();
		}

		public NestedValue getObsoleteExtension() {
			return oboClass.getObsoleteExtension();
		}

		public Collection<Link> getParents() {
			return linkDatabase.getParents(this);
		}

		public Set<PropertyValue> getPropertyValues() {
			return oboClass.getPropertyValues();
		}

		public Set<ObsoletableObject> getReplacedBy() {
			return oboClass.getReplacedBy();
		}

		public NestedValue getReplacedByExtension(ObsoletableObject o) {
			return oboClass.getReplacedByExtension(o);
		}

		public NestedValue getSecondaryIDExtension(String id) {
			return oboClass.getSecondaryIDExtension(id);
		}

		public Set getSecondaryIDs() {
			return oboClass.getSecondaryIDs();
		}

		public Set<Synonym> getSynonyms() {
			return oboClass.getSynonyms();
		}

		public Type<OBOClass> getType() {
			return oboClass.getType();
		}

		public NestedValue getTypeExtension() {
			return oboClass.getTypeExtension();
		}

		public boolean isAnonymous() {
			return oboClass.isAnonymous();
		}

		public boolean isBuiltIn() {
			return oboClass.isBuiltIn();
		}

		public boolean isObsolete() {
			return oboClass.isObsolete();
		}

		public void removeCategory(TermCategory category) {
			oboClass.removeCategory(category);
		}

		public void removeChild(Link tr) {
			oboClass.removeChild(tr);
		}

		public void removeConsiderReplacement(ObsoletableObject o) {
			oboClass.removeConsiderReplacement(o);
		}

		public void removeDbxref(Dbxref xref) {
			oboClass.removeDbxref(xref);
		}

		public void removeDefDbxref(Dbxref dbxref) {
			oboClass.removeDefDbxref(dbxref);
		}

		public void removeParent(Link tr) {
			oboClass.removeParent(tr);
		}

		public void removePropertyValue(PropertyValue pv) {
			oboClass.removePropertyValue(pv);
		}

		public void removeReplacedBy(ObsoletableObject o) {
			oboClass.removeReplacedBy(o);
		}

		public void removeSecondaryID(String secondaryID) {
			oboClass.removeSecondaryID(secondaryID);
		}

		public void removeSynonym(Synonym s) {
			oboClass.removeSynonym(s);
		}

		public void setAnonymousExtension(NestedValue nv) {
			oboClass.setAnonymousExtension(nv);
		}

		public void setComment(String comment) {
			oboClass.setComment(comment);
		}

		public void setCommentExtension(NestedValue nv) {
			oboClass.setCommentExtension(nv);
		}

		public void setDefinition(String definition) {
			oboClass.setDefinition(definition);
		}

		public void setDefinitionExtension(NestedValue nv) {
			oboClass.setDefinitionExtension(nv);
		}

		public void setIDExtension(NestedValue nv) {
			oboClass.setIDExtension(nv);
		}

		public void setIsAnonymous(boolean isAnonymous) {
			oboClass.setIsAnonymous(isAnonymous);
		}

		public void setName(String name) {
			oboClass.setName(name);
		}

		public void setNameExtension(NestedValue nv) {
			oboClass.setNameExtension(nv);
		}

		public void setNamespace(Namespace namespace) {
			oboClass.setNamespace(namespace);
		}

		public void setNamespaceExtension(NestedValue nv) {
			oboClass.setNamespaceExtension(nv);
		}

		public void setObsolete(boolean isObsolete) {
			oboClass.setObsolete(isObsolete);
		}

		public void setObsoleteExtension(NestedValue nv) {
			oboClass.setObsoleteExtension(nv);
		}

		public void setTypeExtension(NestedValue value) {
			oboClass.setTypeExtension(value);
		}
		
		public boolean equals(Object obj) {
			return oboClass.equals(obj);
		}
		
		public int hashCode() {
			return oboClass.hashCode();
		}

		public String getCreatedBy() {
			return oboClass.getCreatedBy();
		}

		public Date getCreationDate() {
			return oboClass.getCreationDate();
		}

		public void setCreatedBy(String username) {
			oboClass.setCreatedBy(username);
		}

		public void setCreationDate(Date date) {
			oboClass.setCreationDate(date);
		}

		public Date getModificationDate() {
			return oboClass.getModificationDate();
		}

		public String getModifiedBy() {
			return oboClass.getModifiedBy();
		}

		public void setModificationDate(Date date) {
			oboClass.setModificationDate(date);
		}

		public void setModifiedBy(String username) {
			oboClass.setModifiedBy(username);
		}

		public NestedValue getCreatedByExtension() {
			return oboClass.getCreatedByExtension();
		}

		public NestedValue getCreationDateExtension() {
			return oboClass.getCreationDateExtension();
		}

		public NestedValue getModificationDateExtension() {
			return oboClass.getModificationDateExtension();
		}

		public NestedValue getModifiedByExtension() {
			return oboClass.getModifiedByExtension();
		}

		public void setCreatedByExtension(NestedValue nv) {
			oboClass.setCreatedByExtension(nv);
		}

		public void setCreationDateExtension(NestedValue nv) {
			oboClass.setCreationDateExtension(nv);
		}

		public void setModificationDateExtension(NestedValue nv) {
			oboClass.setModificationDateExtension(nv);
		}

		public void setModifiedByExtension(NestedValue nv) {
			oboClass.setModifiedByExtension(nv);
		}

	}

	protected SessionHistoryList currentHistory;

	protected Collection<TermCategory> categories;

	protected Collection<SynonymCategory> synonymCategories;

	protected Collection<Namespace> namespaces;

	protected Namespace defaultNamespace;

	protected ObjectFactory objectFactory;

	protected OperationModel operationModel = new DefaultOperationModel() {
		@Override
		protected IdentifiedObject getRealIDObject(String term) {
			return getCachedObject(term, true);
		}
	};

	protected LinkDatabase linkDatabase = new AbstractLinkDatabase() {

		public Collection<Link> getChildren(LinkedObject lo) {
			lo = (LinkedObject) getObject(lo.getID());
			Collection<Link> out = new LinkedList<Link>();
			for (Link link : lo.getChildren()) {
				if (cachedIDHash.containsKey(link.getChild().getID())
						|| cachedIDHash.containsKey(link.getParent().getID())
						|| cachedIDHash.containsKey(link.getType())) {
					Link olink = (Link) link.clone();
					olink.setParent((LinkedObject) getObject(link.getParent()
							.getID()));
					olink.setChild((LinkedObject) getObject(link.getChild()
							.getID()));
					olink.setType((OBOProperty) getObject(link.getType()
							.getID()));
					out.add(olink);
				} else
					out.add(link);
			}
			return out;
		}

		public Collection<Link> getParents(LinkedObject lo) {
			lo = (LinkedObject) WriteCachedOBOSession.this.getUnwrappedObject(lo.getID());
			Collection<Link> out = new LinkedList<Link>();
			for (Link link : lo.getParents()) {
				if (cachedIDHash.containsKey(link.getChild().getID())
						|| cachedIDHash.containsKey(link.getParent().getID())
						|| cachedIDHash.containsKey(link.getType())) {
					Link olink = (Link) link.clone();
					olink.setParent((LinkedObject) WriteCachedOBOSession.this.getUnwrappedObject(link.getParent()
							.getID()));
					olink.setChild((LinkedObject) WriteCachedOBOSession.this.getUnwrappedObject(link.getChild()
							.getID()));
					olink.setType((OBOProperty) WriteCachedOBOSession.this.getUnwrappedObject(link.getType()
							.getID()));
					out.add(olink);
				} else
					out.add(link);
			}
			return out;
		}

		public Collection<IdentifiedObject> getObjects() {
			return WriteCachedOBOSession.this.getObjects();
		}

		public IdentifiedObject getObject(String id) {
			return WriteCachedOBOSession.this.getUnwrappedObject(id);
		}

	};

	protected OBOSession session;

	protected IDProfile profile;
	
	protected Map<String,String> idspaceToUri = new HashMap<String,String>();

	protected String loadRemark;

	protected Map<String, IdentifiedObject> cachedIDHash;

	protected Collection<String> destroyedObjects;

	public WriteCachedOBOSession(OBOSession session) {
		this.session = session;
		operationModel.setSession(this);
		cachedIDHash = new LinkedHashMap<String, IdentifiedObject>();
		destroyedObjects = new LinkedHashSet<String>();
		objectFactory = new DefaultObjectFactory();
		categories = new TinySet<TermCategory>();
		synonymCategories = new TinySet<SynonymCategory>();
		namespaces = new TinySet<Namespace>();
		for (TermCategory cat : session.getCategories()) {
			categories.add((TermCategory) cat.clone());
		}
		for (SynonymCategory cat : session.getSynonymCategories()) {
			synonymCategories.add((SynonymCategory) cat.clone());
		}
		for (Namespace ns : session.getNamespaces()) {
			namespaces.add((Namespace) ns.clone());
		}
		defaultNamespace = session.getDefaultNamespace();
		currentHistory = (SessionHistoryList) session.getCurrentHistory()
				.clone();
		this.profile = session.getIDProfile();
		this.loadRemark = session.getLoadRemark();
	}

	public Collection<TermCategory> getCategories() {
		return categories;
	}

	public void addCategory(TermCategory cat) {
		categories.add(cat);
	}

	public void removeCategory(TermCategory cat) {
		categories.remove(cat);
	}

	public TermCategory getCategory(String name) {
		Iterator it = categories.iterator();
		while (it.hasNext()) {
			TermCategory cat = (TermCategory) it.next();
			if (cat.getName().equals(name))
				return cat;
		}
		return null;
	}

	public void setDefaultNamespace(Namespace ns) {
		this.defaultNamespace = ns;
	}

	public Namespace getDefaultNamespace() {
		return defaultNamespace;
	}

	public void addNamespace(Namespace ns) {
		if (ns == null) {
			(new Exception("Null namespace added")).printStackTrace();
		}
		namespaces.add(ns);
	}

	public Namespace getNamespace(String id) {
		if (id == null)
			return null;
		Iterator it = namespaces.iterator();
		while (it.hasNext()) {
			Namespace ns = (Namespace) it.next();
			if (ns.getID().equals(id))
				return ns;
		}
		return null;
	}

	public void removeNamespace(Namespace ns) {
		namespaces.remove(ns);
	}

	/*
	 * public void setNamespaces(Set namespaces) { this.namespaces = namespaces; }
	 */
	public Collection<Namespace> getNamespaces() {
		return namespaces;
	}

	public Collection<SynonymCategory> getSynonymCategories() {
		return synonymCategories;
	}

	public void addSynonymCategory(SynonymCategory cat) {
		synonymCategories.add(cat);
	}

	public void removeSynonymCategory(SynonymCategory cat) {
		synonymCategories.remove(cat);
	}

	public SynonymCategory getSynonymCategory(String id) {
		Iterator it = synonymCategories.iterator();
		while (it.hasNext()) {
			SynonymCategory cat = (SynonymCategory) it.next();
			if (cat.getID().equals(id))
				return cat;
		}
		return null;
	}

	protected IdentifiedObject getCachedObject(String id, boolean cacheLinks) {
		System.err.println("caching " + id);
		IdentifiedObject out = cachedIDHash.get(id);
		if (out == null) {
			IdentifiedObject real = session.getObject(id);
			if (real == null)
				return null;
			out = (IdentifiedObject) real.clone();
			cachedIDHash.put(out.getID(), out);
		}
		return out;
	}

	protected Collection<IdentifiedObject> objects = new AbstractCollection<IdentifiedObject>() {

		public boolean contains(Object o) {
			if (o instanceof IdentifiedObject) {
				IdentifiedObject io = (IdentifiedObject) o;
				if (cachedIDHash.containsKey(io.getID()))
					return true;
				else
					return !destroyedObjects.contains(io.getID())
							&& session.getObject(io.getID()) != null;
			} else
				return false;
		}

		public boolean isEmpty() {
			return cachedIDHash.size() == 0 && session.getObjects().size() == 0;
		}

		public Iterator<IdentifiedObject> iterator() {
			return new Iterator<IdentifiedObject>() {
				protected IdentifiedObject next;

				protected Iterator<IdentifiedObject> cacheIterator;

				protected Iterator<IdentifiedObject> sessionIterator;

				{
					cacheIterator = cachedIDHash.values().iterator();
					sessionIterator = session.getObjects().iterator();
					cacheNext();
				}

				public boolean hasNext() {
					return next != null;
				}

				private void cacheNext() {
					next = null;
					if (cacheIterator.hasNext()) {
						next = cacheIterator.next();
					} else {
						if (sessionIterator.hasNext()) {
							do {
								next = sessionIterator.next();
								if (cachedIDHash.containsKey(next.getID())
										|| destroyedObjects.contains(next
												.getID()))
									next = null;
							} while (next == null && sessionIterator.hasNext());
						}
					}
				}

				public IdentifiedObject next() {
					IdentifiedObject out = next;
					cacheNext();
					return getObject(out.getID());
				}

				public void remove() {
					throw new UnsupportedOperationException("Not modifiable");
				}

			};
		}

		public int size() {
			int newObjects = 0;
			for (String id : cachedIDHash.keySet()) {
				if (session.getObject(id) == null)
					newObjects++;
			}
			return newObjects + session.getObjects().size()
					- destroyedObjects.size();
		}

		public boolean add(IdentifiedObject o) {
			throw new UnsupportedOperationException("Not modifiable");
		}

		public boolean addAll(Collection<? extends IdentifiedObject> c) {
			throw new UnsupportedOperationException("Not modifiable");
		}

		public void clear() {
			throw new UnsupportedOperationException("Not modifiable");
		}

		public boolean remove(Object o) {
			throw new UnsupportedOperationException("Not modifiable");
		}

		public boolean removeAll(Collection<?> c) {
			throw new UnsupportedOperationException("Not modifiable");
		}

		public boolean retainAll(Collection<?> c) {
			throw new UnsupportedOperationException("Not modifiable");
		}
	};

	public Collection<IdentifiedObject> getObjects() {
		return objects;
	}

	public void removeObject(IdentifiedObject obj) {
		cachedIDHash.remove(obj.getID());
		if (session.getObject(obj.getID()) != null)
			destroyedObjects.add(obj.getID());
	}

	public void addObject(IdentifiedObject obj) {
		cachedIDHash.put(obj.getID(), obj);
		destroyedObjects.remove(obj.getID());
	}
	
	public IdentifiedObject getUnwrappedObject(String id) {
		if (destroyedObjects.contains(id))
			return null;
		IdentifiedObject out = cachedIDHash.get(id);
		if (out == null)
			out = session.getObject(id);
		return out;
		
	}

	public IdentifiedObject getObject(String id) {
		IdentifiedObject out = getUnwrappedObject(id);
		if (out instanceof OBOClass)
			return new CacheWrappedOBOClass((OBOClass) out);
		return out;
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public void addPropertyValue(PropertyValue pv) {
		throw new UnsupportedOperationException(
				"Cannot add property values to a "
						+ "WriteCachedOBOSession - this should only happen at load time, "
						+ "before a WCOS should be created");
	}

	public void addUnknownStanza(UnknownStanza us) {
		throw new UnsupportedOperationException(
				"Cannot add unknown stanzas to a "
						+ "WriteCachedOBOSession - this should only happen at load time, "
						+ "before a WCOS should be created");
	}

	public List<SessionHistoryList> getArchivedHistories() {
		return session.getArchivedHistories();
	}

	public SessionHistoryList getCurrentHistory() {
		return currentHistory;
	}

	public IDProfile getIDProfile() {
		return profile;
	}

	public String getLoadRemark() {
		return loadRemark;
	}

	public ObjectFactory getObjectFactory() {
		return objectFactory;
	}

	public OperationModel getOperationModel() {
		return operationModel;
	}

	public Collection<PropertyValue> getPropertyValues() {
		return session.getPropertyValues();
	}

	public Collection<UnknownStanza> getUnknownStanzas() {
		return session.getUnknownStanzas();
	}

	public HistoryItem importSession(OBOSession session,
			boolean executeImmediately) {
		if (executeImmediately) {
			for (IdentifiedObject lo : session.getObjects()) {
				if (getObject(lo.getID()) == null)
					addObject(lo);
			}
			for (TermCategory cat : session.getCategories()) {
				if (!categories.contains(cat))
					addCategory(cat);
			}
			for (SynonymCategory cat : session.getSynonymCategories()) {
				if (!synonymCategories.contains(cat))
					addSynonymCategory(cat);
			}
			for (Namespace namespace : session.getNamespaces()) {
				if (!namespaces.contains(namespace))
					addNamespace(namespace);
			}
			return null;
		} else {
			throw new UnsupportedOperationException(
					"Undoable import not supported.");
		}
	}

	public void removeUnknownStanza(UnknownStanza us) {
		throw new UnsupportedOperationException(
				"Cannot remove unknown stanzas from a "
						+ "WriteCachedOBOSession");
	}

	public void setIDProfile(IDProfile profile) {
		this.profile = profile;
	}

	public void setLoadRemark(String loadRemark) {
		this.loadRemark = loadRemark;
	}

	public QueryResolver getQueryResolver() {
		return session.getQueryResolver();
	}

	public String getCurrentUser() {
		return session.getCurrentUser();
	}

	public void setCurrentUser(String user) {
		session.setCurrentUser(user);
	}

	public void addIDSpace(String idspace, String uriPrefix) {
		idspaceToUri.put(idspace, uriPrefix);	
	}
	
	public Collection<String> getIDSpaces() {
		return idspaceToUri.keySet();
	}
	
	public String expandIDSpace(String idspace) {
		return idspaceToUri.get(idspace);
	}
}
