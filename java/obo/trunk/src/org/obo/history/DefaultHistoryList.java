package org.obo.history;

import java.util.*;
import java.io.*;

public class DefaultHistoryList implements SessionHistoryList {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6140971918384400136L;

	protected String user;

	protected Date date;

	protected String version;

	protected String comment;

	protected String title;

	protected List<HistoryItem> historyList = new LinkedList<HistoryItem>();

	protected List warnings = new Vector();

	public DefaultHistoryList() {
		date = new Date();
	}

	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public List getWarnings() {
		return warnings;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getUser() {
		return user;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public Iterator getHistoryItems() {
		return historyList.iterator();
	}

	public HistoryItem getItemAt(int index) {
		return historyList.get(index);
	}

	public int getIndex(HistoryItem item) {
		return historyList.indexOf(item);
	}

	public int size() {
		return historyList.size();
	}

	public void addItem(HistoryItem item) {
		if (!(item instanceof EmptyHistoryItem))
			historyList.add(item);
	}

	public void removeItem(HistoryItem item) {
		historyList.remove(item);
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getTitle() {
		if (title != null)
			return title;
		else {
			try {
				java.text.SimpleDateFormat format = new java.text.SimpleDateFormat(
						"hh:mm aaa, MMMMM dd, yyyy");
				return "Saved by " + user + " at " + format.format(date);
			} catch (Exception e) {
				return "History";
			}
		}
	}

	@Override
	public String toString() {
		return getTitle();
	}

	public HistoryList forwardID(String oldID, Collection newIDs) {
		HistoryList out = null;
		Iterator it = historyList.iterator();
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
						out.addItem((HistoryItem) historyList.get(j));
				}
				Iterator it2 = itemList.getHistoryItems();
				while (it2.hasNext()) {
					out.addItem((HistoryItem) it2.next());
				}
			}
		}
		return out;
	}
}
