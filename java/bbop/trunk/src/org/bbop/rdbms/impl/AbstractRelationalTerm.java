package org.bbop.rdbms.impl;

import java.util.Collection;

import org.bbop.rdbms.RelationalTerm;

public abstract class AbstractRelationalTerm implements RelationalTerm {
	public String concat(String c, Collection l) {
		return c;
	}

}
