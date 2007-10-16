package org.obo.datamodel.impl;

import java.util.Collection;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;

public abstract class AbstractLinkDatabase implements LinkDatabase {

	public boolean hasChildren(LinkedObject lo) {
		return getChildren(lo).size() > 0;
	}

	public boolean hasParents(LinkedObject lo) {
		return getParents(lo).size() > 0;
	}

}
