package org.obo.reasoner.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerListener;

/**
 * This ReasonedLinkDatabase will delegate all behavior to the provided
 * ReasonedLinkDatabase. If there is no delegate reasoner, it will pass all
 * plain LinkDatabase behavior to a provided LinkDatabase, and will provide a
 * reasonable no op behavior for everything else.
 * 
 * @author jrichter
 * 
 */
import org.apache.log4j.*;

public class ReasonerPassthroughLinkDatabase implements ReasonedLinkDatabase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerPassthroughLinkDatabase.class);

	protected ReasonedLinkDatabase reasoner;

	protected LinkDatabase linkDatabase;

	protected Collection<ReasonerListener> listeners = new LinkedList<ReasonerListener>();

	public ReasonerPassthroughLinkDatabase(LinkDatabase linkDatabase) {
		setDelegateLinkDatabase(linkDatabase);
	}
	
	public boolean isReasonerEnabled() {
		return reasoner != null;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		for (ReasonerListener listener : listeners) {
			if (this.reasoner != null) {
				this.reasoner.removeReasonerListener(listener);
			}
			reasoner.addReasonerListener(listener);
		}
		this.reasoner = reasoner;
	}
	
	public void cancel() {
		if (reasoner != null)
			reasoner.cancel();
	}
	
	public boolean isRunning() {
		if (reasoner != null)
			return reasoner.isRunning();
		else
			return false;
	}
	
	public boolean isCancelled() {
		if (reasoner != null)
			return reasoner.isCancelled();
		else
			return false;
	}

	public void setDelegateLinkDatabase(LinkDatabase linkDatabase) {
		if (linkDatabase == null)
			throw new IllegalArgumentException("null linkDatabases not allowed");
		this.linkDatabase = linkDatabase;
	}

	public void addLink(Link link) {
		if (reasoner != null)
			reasoner.addLink(link);
	}

	public void addReasonerListener(ReasonerListener listener) {
		listeners.add(listener);
		if (reasoner != null)
			reasoner.addReasonerListener(listener);
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		if (reasoner != null)
			return reasoner.getChildren(lo);
		else
			return linkDatabase.getChildren(lo);
	}

	public Collection<Explanation> getExplanations(PathCapable link) {
		if (reasoner != null)
			return reasoner.getExplanations(link);
		else
			return Collections.emptyList();
	}

	public LinkDatabase getLinkDatabase() {
		if (reasoner != null)
			return reasoner.getLinkDatabase();
		else
			return linkDatabase;
	}

	public IdentifiedObject getObject(String id) {
		if (reasoner != null)
			return reasoner.getObject(id);
		else
			return linkDatabase.getObject(id);
	}

	public Collection<IdentifiedObject> getObjects() {
		if (reasoner != null)
			return reasoner.getObjects();
		else
			return linkDatabase.getObjects();
	}

	public Collection<Link> getParents(LinkedObject lo) {
		if (reasoner != null)
			return reasoner.getParents(lo);
		else
			return linkDatabase.getParents(lo);
	}

	public Collection<LinkedObject> getParentsOfType(LinkedObject a,
			OBOProperty prop) {
		if (reasoner != null)
			return reasoner.getParentsOfType(a, prop);
		else
			return Collections.emptyList();
	}

	public Link hasRelationship(LinkedObject a, OBOProperty prop,
			LinkedObject b) {
		if (reasoner != null)
			return reasoner.hasRelationship(a, prop, b);
		else
			return null;
	}

	public boolean isInstanceOf(Instance a, OBOClass b) {
		if (reasoner != null)
			return reasoner.isInstanceOf(a, b);
		else
			return false;
	}

	public boolean isRedundant(Link link) {
		if (reasoner != null)
			return reasoner.isRedundant(link);
		else
			return false;
	}

	public boolean isSubclassOf(OBOClass a, OBOClass b) {
		if (reasoner != null)
			return reasoner.isSubclassOf(a, b);
		else
			return false;
	}

	public boolean isSubPropertyOf(OBOProperty a, OBOProperty b) {
		if (reasoner != null)
			return reasoner.isSubPropertyOf(a, b);
		else
			return false;
	}

	public long recache() {
		if (reasoner != null)
			return reasoner.recache();
		else
			return 0;
	}

	public void removeLink(Link link) {
		if (reasoner != null)
			reasoner.removeLink(link);
	}

	public void removeReasonerListener(ReasonerListener listener) {
		listeners.remove(listener);
		if (reasoner != null)
			reasoner.removeReasonerListener(listener);
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		if (reasoner != null)
			reasoner.setLinkDatabase(linkDatabase);
	}
	
	public String getProgressString() {
		if (reasoner != null)
			return reasoner.getProgressString();
		else
			return null;
	}
	
	public Number getProgressValue() {
		if (reasoner != null)
			return reasoner.getProgressValue();
		else
			return null;
	}
	
	public boolean hasChildren(LinkedObject lo) {
		if (reasoner != null)
			return reasoner.hasChildren(lo);
		else
			return linkDatabase.hasChildren(lo);
	}
	
	public boolean hasParents(LinkedObject lo) {
		if (reasoner != null)
			return reasoner.hasParents(lo);
		else
			return linkDatabase.hasParents(lo);
	}
}
