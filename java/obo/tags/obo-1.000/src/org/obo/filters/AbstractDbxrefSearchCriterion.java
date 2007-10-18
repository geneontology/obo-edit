package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public abstract class AbstractDbxrefSearchCriterion<T> extends
		AbstractCriterion<T, Dbxref> {

	public Collection<Dbxref> getValues(Collection<Dbxref> scratch, T obj) {
		addDbxrefs(scratch, obj);
		return scratch;
	}

	public Class<Dbxref> getReturnType() {
		return Dbxref.class;
	}

	protected abstract void addDbxrefs(Collection scratch, T io);

	protected void addDbxrefs(Collection scratch, Collection refs) {
		Iterator it = refs.iterator();
		while (it.hasNext())
			addDbxref(scratch, (Dbxref) it.next());
	}

	protected void addDbxref(Collection scratch, Dbxref ref) {
		/*
		if (ref.getDatabase() == null)
			scratch.add(ref.getID());
		else if (ref.getID() == null)
			scratch.add(ref.getDatabase());
		else
			scratch.add(ref.getDatabase() + ":" + ref.getID());
			*/
		scratch.add(ref);
	}

	@Override
	public String toString() {
		return "Dbxref";
	}

	public String getID() {
		return "dbxref";
	}
}
