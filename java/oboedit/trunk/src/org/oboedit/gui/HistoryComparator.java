package org.oboedit.gui;

import org.obo.datamodel.*;
import org.obo.history.SessionHistoryList;

import java.util.Comparator;

public class HistoryComparator implements Comparator<SessionHistoryList> {

	protected static HistoryComparator comparator = new HistoryComparator();

	public static HistoryComparator getComparator() {
		return comparator;
	}

	public int compare(SessionHistoryList a, SessionHistoryList b) {
		return a.getDate().compareTo(b.getDate());
	}
}
