package org.obo.datamodel.impl;

import java.util.*;

import org.obo.datamodel.*;

/**
 * A link database that uses a supplied root algorithm to restrict the visible
 * objects and relationships in the database.
 */

public class RootAlgorithmModeratedLinkDatabase extends MaskedLinkDatabase {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3138882841659656930L;
	protected RootAlgorithm rootAlgorithm;

	public RootAlgorithmModeratedLinkDatabase() {
	}

	public RootAlgorithmModeratedLinkDatabase(LinkDatabase linkDatabase) {
		this(linkDatabase, null);
	}

	public RootAlgorithmModeratedLinkDatabase(LinkDatabase linkDatabase,
			RootAlgorithm rootAlgorithm) {
		super(linkDatabase);
		setRootAlgorithm(rootAlgorithm);
		recache();
	}

	public void setRootAlgorithm(RootAlgorithm rootAlgorithm) {
		this.rootAlgorithm = rootAlgorithm;
	}

	public RootAlgorithm getRootAlgorithm() {
		return rootAlgorithm;
	}

	protected static final Object NONE = new Object();

	@Override
	public void recache() {
		super.recache();
		Iterator it = linkDatabase.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				if (rootAlgorithm.isRoot(lo)) {
					cacheObject(lo);
				}
			}
			/*
			 * System.err.println("caching object "+io); cacheObject(io,
			 * rootMap);
			 */
		}
	}

	protected void cacheObject(LinkedObject lo) {
		setVisible(lo, true);
		Iterator it = linkDatabase.getChildren(lo).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			cacheObject(link.getChild());
		}
	}
}
