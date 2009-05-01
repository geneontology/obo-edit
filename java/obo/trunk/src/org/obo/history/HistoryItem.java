package org.obo.history;

import java.util.*;
import java.io.Serializable;

import org.bbop.util.*;
import org.obo.datamodel.Link;
import org.obo.datamodel.OBORestriction;

public abstract class HistoryItem implements Serializable, Cloneable {

	public static StringRelationship createStringRelationship(Link link) {
		if (link instanceof OBORestriction)
			return new StringRelationship((OBORestriction) link);
		else {
			return new StringRelationship(link.getChild(), link.getType(),
					link.getParent(), false, false, false, null, null, null, link
							.getNamespace().getID());
		}
	}

	protected String target;

	protected HashSet edited = new HashSet();
	protected HashSet editedTerms = new HashSet();

	protected boolean isUndoable = true;
	
	protected Map properties;
	
	public void setProperty(Object key, Object value) {
		if (properties == null) {
			properties = createProperties();
		}
		properties.put(key, value);
	}
	
	protected Map<?,?> createProperties() {	
		return new HashMap();
	}	

	public Object getProperty(Object key) {
		if (properties == null)
			return null;
		else
			return properties.get(key);
	}


	public static int getHash(boolean b) {
		if (b)
			return Boolean.TRUE.hashCode();
		else
			return Boolean.FALSE.hashCode();
	}

	public static int getHash(Object o) {
		if (o == null)
			return 0;
		else
			return o.hashCode();
	}

	public void setTarget(String target) {
		this.target = target;
	}

	public String getTarget() {
		return target;
	}

	public boolean isUndoable() {
		return isUndoable;
	}

	public abstract String getShortName();

	/**
	 * Gets the list of terms that have changed between the two files being compared. 
	 * @return set of edited terms. 
	 */
	public Set getEditedNodes() {
		return (Set) edited.clone();
	}

	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}

	public static HistoryList defaultForwardID(HistoryItem item, String oldID,
			Collection newIDs) {
		if (ObjectUtil.equals(item.getTarget(), oldID)) {
			HistoryList out = new DefaultHistoryList();
			Iterator it = newIDs.iterator();
			while (it.hasNext()) {
				String id = it.next().toString();
				HistoryItem newitem = (HistoryItem) item.clone();
				newitem.setTarget(id);
				out.addItem(newitem);
			}
			return out;
		}
		return null;
	}

	public abstract HistoryList forwardID(String oldID, Collection newIDs);

	public Set getEditedTerms() {
		editedTerms.add(target);
		//System.out.println("HistoryItem: getEditedTerms: target = " + target);
		return (Set) editedTerms.clone();
	}
}
