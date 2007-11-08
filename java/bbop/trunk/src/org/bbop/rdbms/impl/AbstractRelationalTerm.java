package org.bbop.rdbms.impl;

import java.util.Collection;

import org.bbop.rdbms.RelationalTerm;

public abstract class AbstractRelationalTerm implements RelationalTerm {
	public String concat(String c, Collection set) {
		StringBuffer s = new StringBuffer();
		for (Object o : set) {
			if (s.length() == 0) {
				s.append((String)o);
			}
			else {
				s.append(c);
				s.append((String)o);
			}
		}
		return s.toString();
	}
	public String concatValues(String c, Collection set) {
		StringBuffer s = new StringBuffer();
		for (Object o : set) {
			if (s.length() == 0) {
				s.append(dbQuote(o));
			}
			else {
				s.append(c);
				s.append(dbQuote(o));
			}
		}
		return s.toString();
	}
	
	public String dbQuote(Object o) {
		if (o instanceof Integer)
			return o.toString();
		else
			return "'"+o.toString()+"'"; // TODO: make safe
	}

}
