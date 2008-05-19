package org.obo.datamodel.impl;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DefaultLinkDatabase implements LinkDatabase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultLinkDatabase.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -7732358173697641038L;

	protected static final LinkDatabase defaultDatabase = new DefaultLinkDatabase(
			null);

	protected OBOSession session;

	public static LinkDatabase getDefault() {
		return defaultDatabase;
	}

	public DefaultLinkDatabase(OBOSession session) {
		this.session = session;
	}

	public Collection<IdentifiedObject> getObjects() {
		if (session == null)
			return Collections.EMPTY_SET;
		else
			return session.getObjects();
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		return lo.getChildren();
	}

	public Collection<Link> getParents(LinkedObject lo) {
		return lo.getParents();
	}

	public IdentifiedObject getObject(String id) {
		return session.getObject(id);
	}

	public boolean hasChildren(LinkedObject lo) {
		return !lo.getChildren().isEmpty();
	}

	public boolean hasParents(LinkedObject lo) {
		return !lo.getParents().isEmpty();
	}
}
