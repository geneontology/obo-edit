package org.obo.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;

public class HTMLUtil {

	public static String getHTMLLink(IdentifiedObject io, boolean hyperlink) {
		return HTMLUtil.getHTMLLink(null, null, io, hyperlink);
	}

	public static String getHTMLLink(Link link, boolean hyperlink) {
		return HTMLUtil.getHTMLLink(null, null, link, hyperlink);
	}

	public static String getHTMLLink(String param, String val, String desc,
			boolean hyperlink) {
		if (hyperlink)
			return "<a href='file:action"
					+ (param != null ? "(" + param + ")" : "") + "-" + val
					+ "'>" + getHTMLLink(param, val, desc, false) + "</a>";
		else
			return desc;
	}

	public static String getHTMLLink(String param, String desc,
			IdentifiedObject io, boolean hyperlink) {
		if (hyperlink)
			return "<a href='file:term"
					+ (param != null ? "(" + param + ")" : "") + "-"
					+ io.getID().replaceAll(":", "%3A") + "'>"
					+ getHTMLLink(param, desc, io, false) + "</a>";
		else if (desc != null)
			return desc;
		else
			return io.getName() + " <i>(" + io.getID() + ")</i>";
	}

	public static String getHTMLLink(String param, String desc, Link link,
			boolean hyperlink) {
		if (hyperlink)
			return "<a href='file:link"
					+ (param != null ? "(" + param + ")" : "") + "-"
					+ link.getChild().getID().replaceAll(":", "%3A") + "-"
					+ link.getType().getID().replaceAll(":", "%3A") + "-"
					+ link.getParent().getID().replaceAll(":", "%3A") + "'>"
					+ getHTMLLink(param, desc, link, false) + "</a>";
		else if (desc != null)
			return desc;
		else
			return link.getChild() + " -<b>" + link.getType().getID()
					+ "</b>-&gt;" + link.getParent();
	
	}

	public static String removeHyperlinks(String s) {
		Pattern p = Pattern.compile("<a .*?>(.*?)</a>");
		Matcher m = p.matcher(s);
		return m.replaceAll("$1");
	}

}
