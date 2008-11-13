package org.obo.datamodel;

import java.util.*;
import java.io.Serializable;

public interface Synonym extends Cloneable, Serializable, Comparable, IdentifiableObject {

	public final static int UNKNOWN_SCOPE = -1;
	public final static int RELATED_SYNONYM = 0;
	public final static int EXACT_SYNONYM = 1;
	public final static int NARROW_SYNONYM = 2;
	public final static int BROAD_SYNONYM = 3;

	public final static Comparator COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			if (a == null && b == null)
				return 0;
			else if (a == null)
				return -1;
			else if (b == null)
				return 1;
			else {
				return ((Synonym) a).getText().compareToIgnoreCase(
						((Synonym) b).getText());
			}
		}
	};

	public  SynonymType getSynonymType();

	public void setSynonymType(SynonymType type);

	public void setNestedValue(NestedValue nestedValue);

	public NestedValue getNestedValue();

	public int getScope();
	
	public void setScope(int scope);

	public void addXref(Dbxref ref);

	public void removeXref(Dbxref ref);

	public Collection<Dbxref> getXrefs();

	public Object clone();

	public String getText();

	public void setText(String text);
}
