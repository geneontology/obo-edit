package org.obo.datamodel.impl;

import org.bbop.util.*;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DbxrefImpl implements Dbxref {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 7781970775816178223L;
	protected String databaseid;
	protected String database;
	protected String desc;
	protected String id;
	protected static int idgen = 0;

	protected int type;
	protected Synonym targetSynonym;

	protected NestedValue nestedValue;

	public DbxrefImpl() {
		this(null, null);
	}

	public DbxrefImpl(String database, String id) {
		this(database, id, null, ANALOG, null);
	}

	public DbxrefImpl(String database, String id, int type) {
		this(database, id, null, type, null);
	}

	public void setNestedValue(NestedValue nestedValue) {
		this.nestedValue = nestedValue;
	}

	public NestedValue getNestedValue() {
		return nestedValue;
	}

	public static String getTypeStringFromInt(int type) {
		if (type == Dbxref.ANATOMICAL)
			return "anatomical";
		else if (type == Dbxref.RELATED_SYNONYM)
			return "synonym";
		else if (type == Dbxref.DEFINITION)
			return "definition";
		else if (type == Dbxref.ANALOG)
			return "analog";
		else
			return null;
	}

	public DbxrefImpl(String database, String id, String desc, int type) {
		this(database, id, desc, type, null);
	}

	public DbxrefImpl(String database, String databaseid, String desc, int type,
			Synonym targetSynonym) {
		this.databaseid = databaseid;
		this.database = database;
		this.type = type;
		this.desc = desc;
		this.targetSynonym = targetSynonym;
		this.id = ""+idgen++;
	}
	
	public boolean isAnonymous() {
		return true;
	}
	
	public String getID() {
		return id;
	}

	public boolean isDefRef() {
		return type == Dbxref.DEFINITION;
	}

	public void setDesc(String desc) {
		this.desc = desc;
	}

	public String getDesc() {
		return desc;
	}

	public void setType(int type) {
		this.type = type;
	}

	public int getType() {
		return type;
	}

	public void setSynonym(Synonym syn) {
		targetSynonym = syn;
	}

	public Synonym getSynonym() {
		return targetSynonym;
	}

	public String getDatabaseID() {
		return databaseid;
	}

	public String getDatabase() {
		return database;
	}

	public void setDatabaseID(String id) {
		this.databaseid = id;
	}

	public void setDatabase(String database) {
		this.database = database;
	}

	@Override
	public String toString() {
		if (desc == null || desc.length() == 0)
			return database + ":" + databaseid;
		else
			return desc + " (" + database + ":" + databaseid + ")";
	}

	public int compareTo(Object o) {
		return COMPARATOR.compare(this, o);
	}

	@Override
	public boolean equals(Object in) {
		if (in instanceof Dbxref) {
			return ((Dbxref) in).getDatabase().equals(database)
					&& ((Dbxref) in).getDatabaseID().equals(databaseid)
					&& ((Dbxref) in).getType() == type
					&& ObjectUtil.equals(((Dbxref) in).getSynonym(),
							targetSynonym)
					&& ObjectUtil.equals(((Dbxref) in).getDesc(), desc);
		} else
			return false;
	}

	@Override
	public int hashCode() {
		return (database + ":" + databaseid + "|" + type + "|" + targetSynonym + "|" + desc)
				.hashCode();
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}
}
