package org.obo.history;

import java.util.Collection;
import java.util.Iterator;

import org.bbop.util.ObjectUtil;

public abstract class LinkHistoryItem extends HistoryItem {

	protected StringRelationship rel;

	public StringRelationship getRel() {
		return rel;
	}

	public void setRel(StringRelationship rel) {
		this.rel = rel;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o.getClass().equals(getClass())))
			return false;
		LinkHistoryItem item = (LinkHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(rel, item.getRel());
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		if (rel.canForward(oldID) || ObjectUtil.equals(target, oldID)) {
			HistoryList out = new DefaultHistoryList();
			Iterator it = newIDs.iterator();
			while (it.hasNext()) {
				String id = it.next().toString();
				LinkHistoryItem newitem = (LinkHistoryItem) clone();
				newitem.getRel().forwardID(oldID, id);
				if (ObjectUtil.equals(newitem.getTarget(), oldID))
					newitem.setTarget(id);
				out.addItem(newitem);

			}
			return out;
		}
		return null;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(rel);
	}
}
