package org.obo.datamodel.impl;

import java.util.*;

import org.bbop.util.*;
import org.obo.datamodel.*;

public class MergedLinkDatabase implements LinkDatabase {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1566436832426439038L;
	protected Set<LinkDatabase> databases = new LinkedHashSet<LinkDatabase>();

	public Set<LinkDatabase> getDatabases() {
		return databases;
	}

	public void addDatabase(LinkDatabase db) {
		databases.add(db);
	}

	public void removeDatabase(LinkDatabase db) {
		databases.remove(db);
	}

	public Collection<IdentifiedObject> getObjects() {
		if (databases.size() == 1) {
			LinkDatabase db = (LinkDatabase) databases.iterator().next();
			return db.getObjects();
		}
		Superset<IdentifiedObject> out = new FastSuperset<IdentifiedObject>();
		Iterator<LinkDatabase> it = databases.iterator();
		while (it.hasNext()) {
			LinkDatabase db = it.next();
			out.addSubset(db.getObjects());
		}
		return out;
	}

	public Collection<Link> getParents(LinkedObject lo) {
		if (databases.size() == 1) {
			LinkDatabase db = (LinkDatabase) databases.iterator().next();
			return db.getParents(lo);
		}
		Superset<Link> out = new FastSuperset<Link>();
		Iterator<LinkDatabase> it = databases.iterator();
		while (it.hasNext()) {
			LinkDatabase db = it.next();
			out.addSubset(db.getParents(lo));
		}
		return out;
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		if (databases.size() == 1) {
			LinkDatabase db = (LinkDatabase) databases.iterator().next();
			return db.getChildren(lo);
		}
		Superset<Link> out = new FastSuperset<Link>();
		Iterator<LinkDatabase> it = databases.iterator();
		while (it.hasNext()) {
			LinkDatabase db = it.next();
			out.addSubset(db.getChildren(lo));
		}
		return out;
	}

	public IdentifiedObject getObject(String id) {
		Iterator<LinkDatabase> it = databases.iterator();
		while(it.hasNext()) {
			LinkDatabase linkDatabase = it.next();
			IdentifiedObject io = linkDatabase.getObject(id);
			if (io != null)
				return io;
		}
		return null;
	}
	
	public boolean hasChildren(LinkedObject lo) {
		Iterator<LinkDatabase> it = databases.iterator();
		while(it.hasNext()) {
			LinkDatabase linkDatabase = it.next();
			if (linkDatabase.hasChildren(lo))
				return true;
		}
		return false;
	}
	
	public boolean hasParents(LinkedObject lo) {
		Iterator<LinkDatabase> it = databases.iterator();
		while(it.hasNext()) {
			LinkDatabase linkDatabase = it.next();
			if (linkDatabase.hasParents(lo))
				return true;
		}
		return false;
	}
}
