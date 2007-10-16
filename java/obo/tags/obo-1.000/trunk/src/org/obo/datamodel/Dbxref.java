package org.obo.datamodel;

import java.util.*;
import java.io.Serializable;

public interface Dbxref extends Cloneable, Serializable, Comparable, IdentifiableObject {

	public static final Comparator COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			Dbxref ad = (Dbxref) a;
			Dbxref bd = (Dbxref) b;
			int compVal = ad.getDatabase()
					.compareToIgnoreCase(bd.getDatabase());
			if (compVal != 0)
				return compVal;
			compVal = ad.getDatabaseID().compareToIgnoreCase(bd.getDatabaseID());
			if (compVal != 0)
				return compVal;
			if (ad.getDesc() == null) {
				if (bd.getDesc() == null)
					return 0;
				else
					return -1;
			} else if (bd.getDesc() == null)
				return 1;
			else
				return ad.getDesc().compareToIgnoreCase(bd.getDesc());
		}
	};

	public final static int UNKNOWN = -1;

	public final static int ANATOMICAL = 0;

	public final static int RELATED_SYNONYM = 1;

	public final static int DEFINITION = 2;

	public final static int ANALOG = 3;

	public void setNestedValue(NestedValue nestedValue);

	public NestedValue getNestedValue();

	public Object clone();

	public boolean isDefRef();

	public void setDesc(String desc);

	public String getDesc();

	public void setType(int type);

	public int getType();

	public void setSynonym(Synonym syn);

	public Synonym getSynonym();

	public String getDatabaseID();

	public String getDatabase();

	public void setDatabaseID(String id);

	public void setDatabase(String database);
}
