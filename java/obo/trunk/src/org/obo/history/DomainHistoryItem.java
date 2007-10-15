package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;


import java.util.Collection;
import java.util.List;
import java.util.Collections;

public class DomainHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6268415080685362463L;
	protected String domain;
	protected String oldDomain;

	public DomainHistoryItem() {
		this(null, null, null);
	}

	public DomainHistoryItem(OBOProperty target, IdentifiedObject domain) {
		this(target.getID(), (target.getDomain() == null ? null : target
				.getDomain().getID()), (domain == null ? null : domain.getID()));
	}

	public DomainHistoryItem(String target, String oldDomain, String domain) {
		this.target = target;
		this.domain = domain;
		this.oldDomain = oldDomain;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof DomainHistoryItem))
			return false;
		DomainHistoryItem item = (DomainHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(domain, item.getDomain())
				&& ObjectUtil.equals(oldDomain, item.getOldDomain());
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(domain) ^ getHash(oldDomain);
	}

	public void setDomain(String domain) {
		this.domain = domain;
	}

	public void setOldDomain(String oldDomain) {
		this.oldDomain = oldDomain;
	}

	public String getDomain() {
		return domain;
	}

	public String getOldDomain() {
		return oldDomain;
	}

	@Override
	public String toString() {
		return "set domain of " + target + " to " + domain;
	}

	@Override
	public String getShortName() {
		return "change domain";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
