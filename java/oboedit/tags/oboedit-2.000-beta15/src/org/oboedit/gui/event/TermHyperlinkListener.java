package org.oboedit.gui.event;

import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;

public abstract class TermHyperlinkListener implements HyperlinkListener {

	protected LinkDatabase linkDatabase;

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public void hyperlinkUpdate(HyperlinkEvent e) {
		if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
			if (!e.getURL().getProtocol().equals("file"))
				return;
			String path = e.getURL().getPath();
			Pattern paramPattern = Pattern
			.compile("(\\S+?)\\((\\S+?)\\)-(.+)");
			String linkType = null;
			String linkParam = null;
			String linkVal = null;
			Matcher m = paramPattern.matcher(path);
			if (m.matches()) {
				linkType = m.group(1);
				linkParam = m.group(2);
				linkVal = m.group(3);
			} else {
				Pattern noParamPattern = Pattern.compile("(\\S+?)-(.+?)");
				m = noParamPattern.matcher(path);
				if (m.matches()) {
					linkType = m.group(1);
					linkVal = m.group(2);					
				} else
					return;
			}
			System.err.println("linkType = "+linkType);
			System.err.println("linkParam = "+linkParam);
			System.err.println("linkVal = "+linkVal);
			if (linkType.equals("term")) {
				IdentifiedObject io = linkDatabase.getObject(linkVal.replaceAll("%3A", ":"));
				if (io != null)
					termSelected(linkParam, io);
			} else if (linkType.equals("link")) {
				String childID = null;
				String parentID = null;
				String typeID = null;
				StringBuffer temp = new StringBuffer();
				for (int i = 0; i < linkVal.length(); i++) {
					char c = linkVal.charAt(i);
					if (c == '-') {
						if (childID == null)
							childID = temp.toString().replaceAll("%3A", ":");
						else if (typeID == null)
							typeID = temp.toString().replaceAll("%3A", ":");
						temp = new StringBuffer();
					} else if (c == '\\') {
						i++;
						c = linkVal.charAt(i);
						temp.append(c);
					} else
						temp.append(c);
				}
				parentID = temp.toString().replaceAll("%3A", ":");
				IdentifiedObject io = linkDatabase.getObject(childID);
				if (!(io instanceof LinkedObject))
					return;
				LinkedObject child = (LinkedObject) io;
				io = linkDatabase.getObject(parentID);
				if (!(io instanceof LinkedObject)) {
					return;
				}
				LinkedObject parent = (LinkedObject) io;
				io = linkDatabase.getObject(typeID);
				if (!(io instanceof OBOProperty)) {
					return;
				}
				LinkedObject type = (LinkedObject) io;
				Iterator it = linkDatabase.getParents(child).iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					if (link.getParent().equals(parent)
							&& link.getType().equals(type)) {
						linkSelected(linkParam, link);
						return;
					}
				}
			} else if (linkType.equals("action")){
				actionSelected(linkParam, linkVal);
			}
		}
	}

	public abstract void actionSelected(String linkParam, String val);
	public abstract void termSelected(String linkParam, IdentifiedObject io);
	public abstract void linkSelected(String linkParam, Link link);
}
