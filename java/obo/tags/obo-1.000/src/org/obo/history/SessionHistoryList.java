package org.obo.history;

import java.util.Collection;
import java.util.Date;

/**
 * A special {@link HistoryList} that represents a list of edits performed in an
 * editing session. A SessionHistoryList contains additional metadata about the
 * editing session that produced this list of edits.
 * 
 * @author jrichter
 * 
 */
public interface SessionHistoryList extends HistoryList {

	public String getComment();

	public void setComment(String comment);

	public void setVersion(String version);

	public String getVersion();

	public void setDate(Date date);

	public Date getDate();

	public String getUser();

	public void setUser(String user);

	public void setTitle(String title);

	public String getTitle();

	/**
	 * Optional method that maps one id number to another in this HistoryList by
	 * modifying all the history items (and their children) in this list. This
	 * method should only be called by command-line utilities that do id mapping. ID
	 * mapping should never be done within a live OBO session, since most components
	 * depend on ids remaining stable through the duration of a session.
	 * 
	 * This method may be required to map a single id into multiple new ids. If
	 * multiple new ids are specified, new history items will be added to this
	 * HistoryList that reference the new id numbers.
	 * 
	 * @param oldID The id number to remap
	 * @param newIDs One or more id numbers to remap the id to.
	 * @return
	 */
	public HistoryList forwardID(String oldID, Collection newIDs);
}
