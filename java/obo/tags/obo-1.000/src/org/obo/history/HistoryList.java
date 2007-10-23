package org.obo.history;

import java.io.Serializable;
import java.util.*;

/**
 * Represents an ordered list of history items. A special interface is provided
 * for this so that data adapters can provide their own implementations,
 * possibly with real-time communication with some datasource.
 * 
 * @author jrichter
 * 
 */
public interface HistoryList extends Serializable, Cloneable {

	/**
	 * Returns an iterator for the history items in this HistoryList
	 */
	public Iterator getHistoryItems();

	/**
	 * Returns the history item at the given index
	 */
	public HistoryItem getItemAt(int index);

	/**
	 * Returns the index for a given history item. If the item is not in this
	 * history list, return -1.
	 */
	public int getIndex(HistoryItem item);

	/**
	 * Returns the number of HistoryItems in this HistoryList
	 */
	public int size();

	/**
	 * Adds a HistoryItem to the end of this history list
	 */
	public void addItem(HistoryItem item);

	/**
	 * Removes a history item from this history list
	 */
	public void removeItem(HistoryItem item);
	
	/**
	 * Clones this history list. This can safely use the default implementation,
	 * because HistoryItems are read-only once they've been created
	 */
	public Object clone();
}
