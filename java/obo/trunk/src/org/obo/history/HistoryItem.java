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

	/**
	 * Returns a Set of the GO:ids (read from "target" variable) of the terms that have been edited 
	 * between the source and derived file that are being examined 
	 * in the getHistory method.  
	 * 
	 * In this and all override methods the id of a GO term is only added to the editedTerms
	 * collection if the ontology editing step being carried out would alter the content of the term stanza of the term
	 * when it is saved out in OBO-Format. So, for example, if a term was being moved from being a child of
	 * one term to be a child of another term then only the child term id would be added, and not the parent
	 * ids, as this editing step would not alter the stanzas of the parent terms in OBO format. 
	 * 
	 * @return Set editedTerms
	 */
	public Set getEditedTerms() {
		editedTerms.add(target);
		//System.out.println("HistoryItem: getEditedTerms: target = " + target);
		return (Set) editedTerms.clone();
	}
}
