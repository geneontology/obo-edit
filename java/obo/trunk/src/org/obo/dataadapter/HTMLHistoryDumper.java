package org.obo.dataadapter;

import java.io.*;

import org.obo.datamodel.*;
import org.obo.history.*;

import java.util.*;

import org.apache.log4j.*;

public class HTMLHistoryDumper implements HistoryDumper {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HTMLHistoryDumper.class);

	private static final int SPACEPADDING = 2;
	protected HistoryList history;
	protected boolean generateLink = true;

	public void setGenerateLink(boolean generateLink) {
		this.generateLink = generateLink;
	}

	public void dumpHistory(PrintStream stream, HistoryList history)
			throws IOException {
		this.history = history;
		stream.println("<html><body>");
		dumpHistory(stream, history.getHistoryItems(), 0);
		stream.println("</body></html>");
	}

	protected void dumpHistory(PrintStream stream, Iterator iterator,
			int indentLevel) {
		stream.println("<ul>");
		while (iterator.hasNext()) {
			HistoryItem item = (HistoryItem) iterator.next();
			for (int j = 0; j < SPACEPADDING * indentLevel; j++)
				stream.print(" ");
			if (item instanceof TermMacroHistoryItem) {
				stream.println("<ul>");
				dumpHistory(stream, (TermMacroHistoryItem) item,
						indentLevel + 1);
				stream.println("</ul>");
			} else
				stream.println("<li>" + getItemDesc(item, null));
		}
		stream.println("<ul>");
	}

	protected void dumpHistory(PrintStream stream, TermMacroHistoryItem mitem,
			int indentLevel) {
		for (int i = 0; i < mitem.size(); i++) {
			HistoryItem item = mitem.getItemAt(i);
			for (int j = 0; j < SPACEPADDING * indentLevel; j++)
				stream.print(" ");
			if (item instanceof TermMacroHistoryItem) {
				stream.println("<ul>");
				dumpHistory(stream, (TermMacroHistoryItem) item,
						indentLevel + 1);
				stream.println("</ul>");
			} else
				stream.println("<li>" + getItemDesc(item, null));
		}
	}

	public String getLinkFromID(OBOSession history, String id) {
		if (generateLink)
			return "<a href='file:" + id + "'>" + id + " (" + id + ")" + "</a>";
		else
			return id;
	}

	public String getItemDesc(HistoryItem item, OBOSession history) {
		if (item instanceof CreateObjectHistoryItem) {
			return "Created object "
					+ getLinkFromID(history, ((CreateObjectHistoryItem) item)
							.getObjectID()) + " of type "
					+ ((CreateObjectHistoryItem) item).getObjectType() + ".";
		} else if (item instanceof CreateLinkHistoryItem) {
			CreateLinkHistoryItem citem = (CreateLinkHistoryItem) item;
			StringBuffer out = new StringBuffer("");
			out.append("Copied ");

			out.append(getLinkFromID(history, citem.getTarget()) + " (as "
					+ citem.getTypeID() + "), ");

			out.append(" to ");
			out.append(getLinkFromID(history, citem.getParentID()));
			return out.toString();
		} else if (item instanceof TermMoveHistoryItem) {
			StringBuffer out = new StringBuffer("");
			out.append("Moved ");

			StringRelationship tr = ((TermMoveHistoryItem) item)
					.getRelationship();

			out.append(getLinkFromID(history, tr.getChild()) + " (as "
					+ tr.getType() + "), ");
			out.append(" to ");
			out.append(getLinkFromID(history, item.getTarget()));
			out.append(" from ");
			out.append(getLinkFromID(history, tr.getParent()));
			out.append("</body></html>");
			return out.toString();
		} else if (item instanceof DeleteLinkHistoryItem) {
			StringBuffer out = new StringBuffer("");
			StringRelationship tr = ((DeleteLinkHistoryItem) item)
					.getRel();
			out.append("Deleted " + getLinkFromID(history, tr.getChild())
					+ " from " + getLinkFromID(history, tr.getParent())
					+ " with " + tr.getType());
			return out.toString();
		} else
			return item.toString();
	}
}
