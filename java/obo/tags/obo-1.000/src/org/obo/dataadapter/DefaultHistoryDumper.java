package org.obo.dataadapter;

import org.bbop.dataadapter.*;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;

import java.util.*;
import java.io.*;
import java.awt.Color;

public class DefaultHistoryDumper implements OBOEditAdapter, HistoryDumper {

	private static int SPACEPADDING = 2;

	protected AdapterConfiguration config;
	protected String path;

	protected OBOSession history;
	protected IOOperation[] ops = { WRITE_HISTORY };

	protected boolean discardOldHistoriesOnSave = false;
	protected boolean loadAncientHistories = false;
	protected boolean applyAllHistories = false;
	protected boolean applyNewestHistory = false;
	protected boolean applyAllHistoriesAsMacro = false;
	protected boolean applyHistoryAsMacro = false;
	protected List listeners = new Vector();
	protected boolean cancelled = false;

	public void cancel() {
		cancelled = true;
	}

	public DataAdapterUI getPreferredUI() {
		FileAdapterUI ui = new FileAdapterUI();
		ui.setReadOperation(OBOEditAdapter.READ_HISTORY);
		ui.setWriteOperation(OBOEditAdapter.WRITE_HISTORY);
		return ui;
	}

	public void init() {
	}

	public String getName() {
		return "Plaintext History Adapter";
	}

	public String getType() {
		return "Plaintext History files";
	}

	public IOOperation[] getSupportedOperations() {
		return ops;
	}

	public List getHistories() throws DataAdapterException {
		throw new DataAdapterException("History reading is not supported");
	}

	public void writeHistory(HistoryList list) throws DataAdapterException {
		try {
			PrintStream stream = new PrintStream(new FileOutputStream(path));
			dumpHistory(stream, list);
			if (cancelled)
				throw new CancelledAdapterException();
		} catch (IOException ex) {
			throw new DataAdapterException(ex, "Write error");
		}
	}

	public void writeHistories(List list) throws DataAdapterException {
		try {
			PrintStream stream = new PrintStream(new FileOutputStream(path));
			dumpHistories(stream, list, 0);
			if (cancelled)
				throw new CancelledAdapterException();
		} catch (IOException ex) {
			throw new DataAdapterException(ex, "Write error");
		}
	}

	public AdapterConfiguration getConfiguration() {
		return config;
	}

	public Object doOperation(IOOperation op, AdapterConfiguration config,
			Object o) throws DataAdapterException {
		this.config = config;
		cancelled = false;
		if (op.equals(WRITE_HISTORY)) {
			path = ((FileAdapterConfiguration) config).getWritePath();
			List list = new Vector();
			if (o instanceof OBOSession) {
				OBOSession history = (OBOSession) o;
				list.add(history.getCurrentHistory());
				if (!discardOldHistoriesOnSave)
					list.addAll(history.getArchivedHistories());
			} else if (o instanceof HistoryList) {
				list.add(o);
			}
			if (list.size() == 1)
				writeHistory((HistoryList) list.get(0));
			else
				writeHistories(list);
			return null;
		} else if (op.equals(READ_HISTORY)) {
			path = (String) ((FileAdapterConfiguration) config).getReadPaths()
					.iterator().next();
			return getHistories();
		}
		throw new DataAdapterException("Operation " + op + " not supported");
	}

	public void dumpHistories(PrintStream stream, List list, int indentLevel) {
		// printLine(stream, indentLevel, "<histories>");
		Iterator it = list.iterator();
		while (it.hasNext()) {
			HistoryList hl = (HistoryList) it.next();
			dumpHistory(stream, hl.getHistoryItems(), indentLevel + 2);
		}
		// printLine(stream, indentLevel, "</histories>");
	}

	public void setApplyNewestHistory(boolean applyNewestHistory) {
		this.applyNewestHistory = applyNewestHistory;
	}

	public boolean applyNewestHistory() {
		return applyNewestHistory;
	}

	public void setDiscardOldHistoriesOnSave(boolean discard) {
		this.discardOldHistoriesOnSave = discard;
	}

	public boolean discardOldHistoriesOnSave() {
		return discardOldHistoriesOnSave;
	}

	public boolean loadAncientHistories() {
		return loadAncientHistories;
	}

	public void setLoadAncientHistories(boolean loadAncientHistories) {
		this.loadAncientHistories = loadAncientHistories;
	}

	public boolean applyAllHistories() {
		return applyAllHistories;
	}

	public void setApplyAllHistories(boolean applyAllHistories) {
		this.applyAllHistories = applyAllHistories;
	}

	public boolean applyAllHistoriesAsMacro() {
		return applyAllHistoriesAsMacro;
	}

	public void setApplyAllHistoriesAsMacro(boolean applyAllHistoriesAsMacro) {
		this.applyAllHistoriesAsMacro = applyAllHistoriesAsMacro;
	}

	public void setApplyHistoryAsMacro(boolean applyHistoryAsMacro) {
		this.applyHistoryAsMacro = applyHistoryAsMacro;
	}

	public boolean applyHistoryAsMacro() {
		return applyHistoryAsMacro;
	}

	public void setDEHistory(OBOSession history) {
		this.history = history;
	}

	public void dumpHistory(PrintStream stream, OBOSession history)
			throws IOException {
		this.history = history;
		dumpHistory(stream, history.getCurrentHistory());
	}

	public void dumpHistory(PrintStream stream, HistoryList list) {
		dumpHistory(stream, list.getHistoryItems(), 0);
	}

	protected void dumpHistory(PrintStream stream, Iterator iterator,
			int indentLevel) {
		// for(int i=0; i < historyList.size(); i++) {
		while (iterator.hasNext() && !cancelled) {
			HistoryItem item = (HistoryItem) iterator.next();
			for (int j = 0; j < SPACEPADDING * indentLevel; j++)
				stream.print(" ");
			stream.println("* " + getItemDesc(item, history));
			if (item instanceof TermMacroHistoryItem)
				dumpHistory(stream, (TermMacroHistoryItem) item,
						indentLevel + 1);
		}
	}

	protected void dumpHistory(PrintStream stream, TermMacroHistoryItem mitem,
			int indentLevel) {
		for (int i = 0; i < mitem.size(); i++) {
			HistoryItem item = mitem.getItemAt(i);
			for (int j = 0; j < SPACEPADDING * indentLevel; j++)
				stream.print(" ");
			stream.println("* " + getItemDesc(item, history));
			if (item instanceof TermMacroHistoryItem)
				dumpHistory(stream, (TermMacroHistoryItem) item,
						indentLevel + 1);
		}
	}

	protected String getTerm(String id) {
		if (history == null) {
			return id;
		} else {
			IdentifiedObject term = history.getObject(id);
			if (term == null)
				return id;
			else
				return getTermString(term);
		}
	}

	protected String getTermString(IdentifiedObject term) {
		return term.getName() + " (" + term.getID() + ")";
	}

	public String getItemDesc(HistoryItem item, OBOSession history) {
		/*
		 * if (item instanceof CreateObjectHistoryItem) { return "Created object
		 * "+((CreateObjectHistoryItem) item). getObjectID()+" of type "+
		 * ((CreateObjectHistoryItem) item).getObjectType()+ "."; } else if
		 * (item instanceof TermCopyHistoryItem) { StringBuffer out = new
		 * StringBuffer(""); out.append("Copied ");
		 * 
		 * HistoryItem.StringRelationship tr = ((TermCopyHistoryItem)
		 * item).getTerm();
		 * 
		 * out.append(getTerm(tr.getChild())+ " (as "+tr.getType()+"),");
		 * 
		 * out.append(" to "); out.append(getTerm(item.getTarget())); return
		 * out.toString(); } else if (item instanceof TermMoveHistoryItem) {
		 * StringBuffer out = new StringBuffer(""); out.append("Moved ");
		 * 
		 * HistoryItem.StringRelationship tr = ((TermMoveHistoryItem)
		 * item).getRelationship();
		 * 
		 * out.append(getTerm(tr.getChild())+ " (as "+ tr.getType()+ "), from
		 * "+getTerm(tr.getParent()));
		 * 
		 * out.append(" to "); out.append(getTerm(item.getTarget())); return
		 * out.toString(); } else if (item instanceof DeleteLinkHistoryItem) {
		 * StringBuffer out = new StringBuffer("");
		 * HistoryItem.StringRelationship tr = ((DeleteLinkHistoryItem)
		 * item).getRel(); out.append("Deleted "+getTerm(tr.getChild())+ " from "+
		 * getTerm(tr.getParent())+ " with "+ tr.getType()); return
		 * out.toString(); } else
		 */
		return item.toString();
	}

	public String getID() {
		return "OBOEDIT:History_Dumper";
	}

	public Properties getStateInformation() {
		return null;
	}

	public void setStateInformation(Properties props) {
	}


	public String getProgressString() {
		return null;
	}

	public Number getProgressValue() {
		return null;
	}
}
