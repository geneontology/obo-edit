package org.obo.datamodel;

import java.io.Serializable;
import java.util.Comparator;

public class Namespace implements Serializable, Cloneable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static int idgen = 0;
	protected int private_id = 0;
	protected String id;
	protected String path;

	public final static Comparator COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			if (a == null && b == null)
				return 0;
			else if (a == null)
				return -1;
			else if (b == null)
				return 1;
			else {
				return ((Namespace) a).getID().compareToIgnoreCase(
						((Namespace) b).getID());
			}
		}
	};

	public Namespace(String path) {
		this.path = path;
		this.id = path;
		private_id = idgen++;
	}

	public Namespace() {
		this(null, null);
	}

	public Namespace(String id, String path) {
		this.id = id;
		this.path = path;
		private_id = idgen++;
	}

	public void setID(String id) {
		this.id = id;
	}

	public String getID() {
		return id;
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			return null;
		}
	}

	/*
	 * public String getExplicitID() { return id; }
	 */
	public String getPath() {
		return path;
	}

	@Override
	public String toString() {
		return getID();
	}

	@Override
	public int hashCode() {
		return getID().hashCode();
	}

	public int getPrivateID() {
		return private_id;
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof Namespace) {
			Namespace ns = (Namespace) o;
			return ns.getID().equals(getID());
		} else
			return false;
	}
}
