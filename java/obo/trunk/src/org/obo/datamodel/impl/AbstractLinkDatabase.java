package org.obo.datamodel.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.util.TermUtil;

public abstract class AbstractLinkDatabase implements LinkDatabase {
	
	protected Collection<OBOProperty> properties = new HashSet<OBOProperty>();
	OBOSession session;


	public boolean hasChildren(LinkedObject lo) {
		return getChildren(lo).size() > 0;
	}

	public boolean hasParents(LinkedObject lo) {
		return getParents(lo).size() > 0;
	}

	public Link hasRelationship(LinkedObject a, OBOProperty b, LinkedObject c) {
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getType().equals(b)
					&& link.getParent().equals(c)) {
				return link;
			}
		}
		return null;
	}

	public Collection<Link> getLinks(OBOProperty p) {
		Collection<Link> links = new ArrayList<Link>();
		for (IdentifiedObject io : getObjects()) {
			if (io instanceof LinkedObject) {
				for (Link link : getParents((LinkedObject) io)) {
					if (link.getType().equals(p)) {
						links.add(link);
					}
				}
			}
		}
		return links;
	}

	public Collection<OBOProperty> getProperties() {
		return properties;
	}

	public void setProperties(Collection<OBOProperty> properties) {
		this.properties = properties;
	}

	public OBOSession getSession() {
		return session;
	}

	public void setSession(OBOSession session) {
		this.session = session;
	}

}
