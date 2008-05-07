package org.obo.datamodel.impl;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.*;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.history.DefaultHistoryList;
import org.obo.history.HistoryItem;
import org.obo.history.OperationModel;
import org.obo.history.SessionHistoryList;
import org.obo.identifier.IDProfile;
import org.obo.query.QueryResolver;
import org.obo.query.impl.DefaultQueryResolver;
import org.obo.util.TermUtil;

public class OBOSessionImpl implements OBOSession {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6487885507983696952L;
	protected static transient OBOSessionImpl serializingSession;

	protected SessionHistoryList currentHistory;
	protected List<SessionHistoryList> archivedHistories;
	protected ObjectFactory objectFactory;
	protected LinkDatabase linkDatabase = new DefaultLinkDatabase(this);
	protected Pattern p = Pattern
			.compile("(.+)([-|~])(.+)[-|~]>(.+)");
	protected Collection<TermCategory> categories;
	protected Collection<SynonymCategory> synonymCategories;
	protected Collection<Namespace> namespaces;
	protected Map<String, IdentifiedObject> idHash;
	protected Collection<PropertyValue> propertyValues;
	protected Collection<UnknownStanza> unknownStanzas;
	protected Namespace defaultNamespace;
	protected IDProfile profile;
	
	protected Map<String,String> idspaceToUri = new HashMap<String,String>();

	protected String loadRemark;
	protected OperationModel operationModel;
	protected String currentUser;

	public void setIDProfile(IDProfile profile) {
		this.profile = profile;
	}

	public IDProfile getIDProfile() {
		return profile;
	}

	public List<SessionHistoryList> getArchivedHistories() {
		return archivedHistories;
	}

	public String getLoadRemark() {
		return loadRemark;
	}

	public void setLoadRemark(String loadRemark) {
		this.loadRemark = loadRemark;
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

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public OBOSessionImpl() {
		this(new DefaultObjectFactory());
	}

	public OBOSessionImpl(ObjectFactory factory) {
		currentHistory = new DefaultHistoryList();
		operationModel = new DefaultOperationModel();
		operationModel.setSession(this);
		categories = new TinySet<TermCategory>();
		synonymCategories = new TinySet<SynonymCategory>();
		namespaces = new TinySet<Namespace>();
		propertyValues = new TinySet<PropertyValue>();
		unknownStanzas = new TinySet<UnknownStanza>();
		idHash = new LinkedHashMap<String, IdentifiedObject>();
		archivedHistories = new LinkedList<SessionHistoryList>();
		for (int i = 0; i < OBOClass.BUILTIN_CLASSES.length; i++)
			addObject((OBOClass) OBOClass.BUILTIN_CLASSES[i].clone());

		for (int i = 0; i < OBOProperty.BUILTIN_TYPES.length; i++)
			addObject((OBOProperty) OBOProperty.BUILTIN_TYPES[i].clone());

		for (int i = 0; i < Datatype.DATATYPES.length; i++)
			addObject(Datatype.DATATYPES[i]);

		this.objectFactory = factory;
	}

	/**
	 * This method changes all references to an id in this OBOSession to another
	 * id. This method is bad news, and should never be used unless it is
	 * absolutely certain that the old identifier is not part of a published
	 * ontology. Stable identifiers are crucial in public ontologies!
	 * 
	 * This method is not part of the {@link OBOSession} interface because not
	 * all OBOSession implementations can even support this operation; it
	 * involves quite a bit of mucking around with the internal implementations
	 * of the datamodels. Thus, this method only works if we can be sure we're
	 * working with an {@link AnnotatedObjectImpl}
	 * 
	 * Never use this method as part of a live GUI editing session, or while
	 * using the reasoner. The reasoner, the gui, the history system, and almost
	 * everything else expects identifiers to remain stable. If an identifier is
	 * changed during a live session, all kinds of unexpected results will
	 * occur.
	 * 
	 * @param object
	 * @param newID
	 */
	public void changeID(AnnotatedObjectImpl object, String newID) {
		/*
		 * To implement this, we first remove all references to the object from
		 * the session and its parents and children. We then change the id, and
		 * add the object back. The object has to be removed and added because
		 * the OBOSession and LinkedObject implementations store ontology
		 * objects in a hash of some kind. Since any implementation of
		 * IdentifiedObject should have the equals() and hashCode() methods
		 * based on the id, changing the id while the object is stored in a hash
		 * will leave the hash in an inconsistent state.
		 */

		// remove the object from the ontology
		removeObject(object);
		LinkedList<Link> parents = null;
		LinkedList<Link> children = null;
		if (object instanceof LinkedObject) {
			// and remove its parents and children if applicable
			LinkedObject lobj = (LinkedObject) object;
			parents = new LinkedList<Link>();
			children = new LinkedList<Link>();
			parents.addAll(lobj.getParents());
			children.addAll(lobj.getChildren());
			Iterator it = parents.iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				lobj.removeParent(link);
			}
			it = children.iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				lobj.removeChild(link);
			}
		}
		// change the id
		object.id = newID;
		// add the object back to the ontology
		addObject(object);
		if (object instanceof LinkedObject) {
			// restore its parents and children
			LinkedObject lobj = (LinkedObject) object;
			Iterator it = parents.iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				lobj.addParent(link);
			}
			it = children.iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				lobj.addChild(link);
			}
		}
	}

	public SessionHistoryList getCurrentHistory() {
		return currentHistory;
	}

	public void addPropertyValue(PropertyValue pv) {
		propertyValues.add(pv);
	}

	public Collection<PropertyValue> getPropertyValues() {
		return propertyValues;
	}

	public void addUnknownStanza(UnknownStanza us) {
		unknownStanzas.add(us);
	}

	public Collection<UnknownStanza> getUnknownStanzas() {
		return unknownStanzas;
	}

	public IdentifiedObject getObject(String id) {
		if (id == null)
			return null;
		IdentifiedObject out = (IdentifiedObject) idHash.get(id);
		if (out != null)
			return out;
		Matcher m = p.matcher(id);
		if (m.matches()) {
			String childID = m.group(1);
			String linkTypeStr = m.group(2);
			String typeID = m.group(3);
			String parentID = m.group(4);
			
			boolean completes = linkTypeStr.equals("~");
			IdentifiedObject childObj;
			IdentifiedObject parentObj;
			IdentifiedObject typeObj;
			if (childID.equals("null"))
				childObj = null;
			else
				childObj = (IdentifiedObject) idHash.get(childID);
			if (parentID.equals("null"))
				parentObj = null;
			else
				parentObj = (IdentifiedObject) idHash.get(parentID);
			if (typeID.equals("null"))
				typeObj = null;
			else
				typeObj = (IdentifiedObject) idHash.get(typeID);
			if (childObj instanceof LinkedObject
					&& parentObj instanceof LinkedObject
					&& typeObj instanceof OBOProperty) {
				for (Link link : ((LinkedObject) childObj).getParents()) {
					if (link.getType().equals(typeObj)
							&& link.getParent().equals(parentObj) &&
							TermUtil.isIntersection(link) == completes) {
						return new LinkLinkedObject(link);
					}
				}
			}
		}
		return null;
	}

	public ObjectFactory getObjectFactory() {
		return objectFactory;
	}

	public void addObject(IdentifiedObject obj) {
		idHash.put(obj.getID(), obj);
	}

	public void removeObject(IdentifiedObject obj) {
		idHash.remove(obj.getID());
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

	protected void indexObject(IdentifiedObject o) {
		idHash.put(o.getID(), o);
	}

	public Collection<IdentifiedObject> getObjects() {
		return idHash.values();
	}

	private synchronized void readObject(java.io.ObjectInputStream in) throws IOException,
			ClassNotFoundException {
		serializingSession = this;
		in.defaultReadObject();
		serializingSession = null;
/*
		List<Link> tempList = new LinkedList<Link>();

		Iterator it = getObjects().iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) o;
				tempList.addAll(lo.getParents());

				// tempList.addAll(lo.getChildren());
				Iterator it2 = lo.getParents().iterator();
				while (it2.hasNext()) {
					Object o2 = it2.next();
					if (o2 instanceof OBORestrictionImpl) {
						OBORestrictionImpl or = (OBORestrictionImpl) o2;
						or.parent = (LinkedObject) getObject(or.parent_id);
						or.child = (LinkedObject) getObject(or.child_id);
						or.type = (OBOProperty) getObject(or.type_id);
					}
				}
				lo.getChildren().clear();
				lo.getParents().clear();
			}
		}
		it = tempList.iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			link.getChild().getParents().add(link);
			link.getParent().getChildren().add(link);
		}
		*/
	}

	public void removeUnknownStanza(UnknownStanza us) {
		unknownStanzas.remove(us);		
	}

	public HistoryItem importSession(OBOSession session, boolean executeNow) {
		if (executeNow) {
			for(IdentifiedObject lo : session.getObjects()) {
				if (idHash.containsKey(lo.getID())) {
					IdentifiedObject curr = idHash.get(lo.getID());
					if (curr instanceof DanglingObject) {
						addObject(lo);
					}
				}
				else {
					// TODO - merge objects
					if (!idHash.containsKey(lo.getID()))
						addObject(lo);
				}

			}
			for(TermCategory cat : session.getCategories()) {
				if (!categories.contains(cat))
					addCategory(cat);
			}
			for(SynonymCategory cat : session.getSynonymCategories()) {
				if (!synonymCategories.contains(cat))
					addSynonymCategory(cat);
			}
			for(Namespace namespace : session.getNamespaces()) {
				if (!namespaces.contains(namespace))
					addNamespace(namespace);
			}
			TermUtil.resolveDanglingLinks(this);
			return null;
		} else {
			throw new UnsupportedOperationException("Undoable import not supported.");
		}
	}

	public OperationModel getOperationModel() {
		return operationModel;
	}

	public QueryResolver getQueryResolver() {
		return null;
	}

	public String getCurrentUser() {
		return currentUser;
	}

	public void setCurrentUser(String currentUser) {
		this.currentUser = currentUser;
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
