package org.obo.dataadapter;

import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;

import java.io.*;

public interface HistoryDumper {

	public void dumpHistory(PrintStream stream, HistoryList history)
			throws IOException;

	public String getItemDesc(HistoryItem item, OBOSession history);
}
