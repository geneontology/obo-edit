package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import javax.naming.OperationNotSupportedException;
import javax.swing.tree.*;

import org.apache.log4j.*;

public class TermMacroHistoryItem extends HistoryItem implements HistoryList {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermMacroHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -9191451800260864200L;

	protected List<HistoryItem> historyItems = new LinkedList<HistoryItem>();

	protected String description;

	protected Vector rels;

	protected String result;

	protected boolean locked = false;

	public TermMacroHistoryItem() {
		this((String) null);
	}

	public TermMacroHistoryItem(String description) {
		this.description = description;
		rels = new Vector();
	}
	
	public TermMacroHistoryItem(List<HistoryItem> changes) {
		this(null, changes);
	}	

	public TermMacroHistoryItem(String description, List<HistoryItem> changes) {
		this((String) null);
		for(HistoryItem item : changes) {
			addItem(item);
		}
	}
	
	public TermMacroHistoryItem(String description, HistoryList historyList) {
		this((String) null);
		Iterator it = historyList.getHistoryItems();
		while(it.hasNext()) {
			addItem((HistoryItem) it.next());
		}
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermMacroHistoryItem))
			return false;
		TermMacroHistoryItem item = (TermMacroHistoryItem) o;
		if (size() != item.size())
			return false;
		for (int i = 0; i < size(); i++) {
			HistoryItem item2 = getItemAt(i);
			HistoryItem item3 = item.getItemAt(i);
			if (!ObjectUtil.equals(item3, item2))
				return false;
		}
		return true;
	}

	@Override
	public int hashCode() {
		int out = 0;
		for (int i = 0; i < size(); i++) {
			HistoryItem item = getItemAt(i);
			out ^= item.hashCode();
		}
		return out;
	}

	@Override
	public String getShortName() {
		return description;
	}

	public void setTarget(OBOClass target) {
		setTarget(target.getID());
	}

	@Override
	public void setTarget(String target) {
		this.target = target;
	}

	public void setResult(String result) {
		this.result = result;
	}

	public void setResult(OBOClass term) {
		result = term.getID();
	}

	public String getResult() {
		return result;
	}

	public Vector getSources() {
		return rels;
	}

	public void setSources(TreePath[] paths) {
		for (int i = 0; i < paths.length; i++) {
			Link tr = (Link) paths[i].getLastPathComponent();
			rels.add(HistoryItem.createStringRelationship(tr));
		}
	}

	public void setSource(Link tr) {
		Vector rels = new Vector();
		rels.add(tr);
		setSources(rels);
	}

	public void setSources(Vector rels) {
		Vector out = new Vector();
		for (int i = 0; i < rels.size(); i++) {
			Link tr = (Link) rels.get(i);
			out.add(HistoryItem.createStringRelationship(tr));
		}
		this.rels = out;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int size() {
		return historyItems.size();
	}

	public HistoryItem getItemAt(int index) {
		return historyItems.get(index);
	}

	public void setHistoryItems(Vector items) {
		if (locked)
			throw new IllegalStateException("Cannot modify a locked macro "
					+ "history item");
		this.historyItems = items;
	}

	public OperationWarning lock(OBOSession history) {
		boolean failure = false;
		OperationWarning warning = new OperationWarning("Couldn't lock "
				+ "macro history item " + toString());
		for (int i = 0; i < historyItems.size(); i++) {
			HistoryItem item = (HistoryItem) historyItems.get(i);
			if (item instanceof TermMacroHistoryItem) {
				OperationWarning ow = ((TermMacroHistoryItem) item)
						.lock(history);
				if (ow != null) {
					failure = true;
					warning.addWarning(ow);
				}
			}
		}
		if (failure)
			return warning;
		else {
			locked = true;
			return null;
		}
	}

	@Override
	public Set getEditedNodes() {
		edited = new HashSet();
		if (target != null)
			edited.add(target);
		if (result != null)
			edited.add(result);
		for (int i = 0; i < rels.size(); i++) {
			StringRelationship sr = (StringRelationship) rels.get(i);
			edited.add(sr.getChild());
			edited.add(sr.getParent());
		}
		for (int i = 0; i < historyItems.size(); i++) {
			edited.addAll(((HistoryItem) historyItems.get(i)).getEditedNodes());
		}
		return edited;
	}

	public void setHistoryList(HistoryList list) {
		Iterator it = list.getHistoryItems();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();
			addItem(item);
		}
	}

	public void addItem(HistoryItem item) {
		if (locked)
			throw new IllegalStateException("Cannot modify a locked macro "
					+ "history item");
		historyItems.add(item);
	}

	public void removeItem(HistoryItem item) {
		if (locked)
			throw new IllegalStateException("Cannot modify a locked macro "
					+ "history item");
		historyItems.remove(item);
	}

	@Override
	public String toString() {
//		return (description != null ? description + " (macro)"
//				: "unnamed macro");
	    return historyItems.toString();
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		HistoryList out = null;
		Iterator it = historyItems.iterator();
		for (int i = 0; it.hasNext(); i++) {
			HistoryItem item = (HistoryItem) it.next();
			HistoryList itemList = item.forwardID(oldID, newIDs);
			if (itemList == null) {
				if (out != null)
					out.addItem(item);
			} else {
				if (out == null) {
					out = new DefaultHistoryList();
					for (int j = 0; j < i; j++)
						out.addItem((HistoryItem) historyItems.get(j));
				}
				Iterator it2 = itemList.getHistoryItems();
				while (it2.hasNext()) {
					out.addItem((HistoryItem) it2.next());
				}
			}
		}
		return out;
	}

	public String getComment() {
		throw new UnsupportedOperationException(
				"getComment() not supported for TermMacroHistoryItem");
	}

	public Date getDate() {
		throw new UnsupportedOperationException(
				"getDate() not supported for TermMacroHistoryItem");
	}

	public Iterator getHistoryItems() {
		return historyItems.iterator();
	}

	public int getIndex(HistoryItem item) {
		return historyItems.indexOf(item);
	}

	public String getTitle() {
		throw new UnsupportedOperationException(
				"getDate() not supported for TermMacroHistoryItem");
	}

	public String getUser() {
		throw new UnsupportedOperationException(
				"getDate() not supported for TermMacroHistoryItem");
	}

	public String getVersion() {
		throw new UnsupportedOperationException(
				"getDate() not supported for TermMacroHistoryItem");
	}

	public List getWarnings() {
		throw new UnsupportedOperationException(
				"getWarnings() not supported for TermMacroHistoryItem");
	}

	public void setComment(String comment) {
		throw new UnsupportedOperationException(
				"setComment() not supported for TermMacroHistoryItem");
	}

	public void setDate(Date date) {
		throw new UnsupportedOperationException(
				"setDate() not supported for TermMacroHistoryItem");
	}

	public void setTitle(String title) {
		throw new UnsupportedOperationException(
				"setTitle() not supported for TermMacroHistoryItem");
	}

	public void setUser(String user) {
		throw new UnsupportedOperationException(
				"setUser() not supported for TermMacroHistoryItem");
	}

	public void setVersion(String version) {
		throw new UnsupportedOperationException(
				"setVersion() not supported for TermMacroHistoryItem");
	}
	
	/**
	 * Returns an empty copy of the Set editedTerms. The parent
	 * version of this method is overridden to return an empty set as 
	 * the sub-class does not contain a variable that represents
	 * the GO:id of a term that has been edited or would be useful to return. 
	 *
	 * @return Set editedTerms
	 */
	@Override
	public Set getEditedTerms() {
		return (Set) editedTerms.clone();
	}
}
