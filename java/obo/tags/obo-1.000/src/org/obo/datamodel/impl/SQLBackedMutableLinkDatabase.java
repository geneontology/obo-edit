package org.obo.datamodel.impl;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Properties;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.IdentifiedObjectIndex;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MutableLinkDatabase;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;

public class SQLBackedMutableLinkDatabase extends AbstractLinkDatabase
		implements MutableLinkDatabase {

	protected String dbName = "ReasonerCache";

	protected Properties dbProperties = new Properties();

	protected Connection dbConnection = null;

	protected Statement stmt;

	protected IdentifiedObjectIndex index;

	public SQLBackedMutableLinkDatabase(IdentifiedObjectIndex index) {
		initializeDatabase();
		setIdentifiedObjectIndex(index);
	}

	public void setIdentifiedObjectIndex(IdentifiedObjectIndex index) {
		this.index = index;
	}

	protected void initializeDatabase() {
		// decide on the db system directory
		String userHomeDir = System.getProperty("user.home", ".");
		String systemDir = userHomeDir + "/." + dbName;
		System.setProperty("database.system.home", systemDir);

		// create the db system directory
		File fileSystemDir = new File(systemDir);
		fileSystemDir.mkdir();
		try {
			Class.forName("org.hsqldb.jdbcDriver");
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		}
		initializeConnection(dbExists());
	}

	protected boolean createTables(Statement stmt) {
		boolean bCreatedTables = false;
		try {
			stmt.execute(strCreateReasonerTable);
			bCreatedTables = true;
		} catch (SQLException ex) {
		}

		return bCreatedTables;
	}

	protected static final String strCreateReasonerTable = "create table LINKS ("
			+ "    CHILD_ID          VARCHAR(30) NOT NULL,"
			+ "    TYPE_ID    VARCHAR(30) NOT NULL, "
			+ "    PARENT_ID   VARCHAR(30) NOT NULL, "
			+ "    constraint pk_iot_ primary key (CHILD_ID, TYPE_ID, PARENT_ID)"
			+ ")";

	protected boolean initializeConnection(boolean dbExists) {
		boolean bCreated = false;

		String dbUrl = getDatabaseURL();
		dbProperties.put("create", !dbExists + "");

		try {
			dbConnection = DriverManager.getConnection(dbUrl, dbProperties);
			dbConnection.setAutoCommit(true);
			stmt = dbConnection.createStatement();
			bCreated = createTables(stmt);
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
		dbProperties.remove("create");
		return bCreated;
	}

	public String getDatabaseURL() {
		return "jdbc:hsqldb:file:" + getDatabaseLocation();
	}

	public String getDatabaseLocation() {
		String dbLocation = System.getProperty("database.system.home");
		return dbLocation;
	}

	protected boolean dbExists() {
		boolean bExists = false;
		String dbLocation = getDatabaseLocation();
		File dbFileDir = new File(dbLocation);
		if (dbFileDir.exists()) {
			bExists = true;
		}
		return bExists;
	}

	public void addObject(IdentifiedObject lo) {
	}

	public synchronized void clearObjectsAndAddParents(
			Collection<LinkedObject> clearUs, Collection<Link> links) {
		try {
			for (LinkedObject clearMe : clearUs) {
				stmt.execute("delete from LINKS where CHILD_ID = '"
						+ clearMe.getID() + "'");
			}
			for (Link link : links) {
				stmt.execute("insert into LINKS values ('"
						+ link.getChild().getID() + "', '"
						+ link.getType().getID() + "', '"
						+ link.getParent().getID() + "')");
			}
			dbConnection.commit();
		} catch (SQLException e) {
			System.err.println("failed insert of multiple links");
			e.printStackTrace();
		}

	}

	public void addParents(Collection<Link> links) {
		try {
			for (Link link : links) {
				stmt.execute("insert into LINKS values ('"
						+ link.getChild().getID() + "', '"
						+ link.getType().getID() + "', '"
						+ link.getParent().getID() + "')");
			}
			dbConnection.commit();
		} catch (SQLException e) {
			System.err.println("failed insert of multiple links");
			e.printStackTrace();
		}
	}

	public synchronized void addParent(Link link) {
		addParents(Collections.singleton(link));
	}

	public synchronized void setParents(LinkedObject lo, Collection<Link> links) {
		try {
			stmt.execute("delete from LINKS where CHILD_ID = '" + lo.getID()
					+ "'");
		} catch (SQLException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		for (Link link : links) {
			try {
				stmt.execute("insert into LINKS values ('"
						+ link.getChild().getID() + "', '"
						+ link.getType().getID() + "', '"
						+ link.getParent().getID() + "')");
			} catch (SQLException e) {
				System.err.println("failed insert of " + link);
				e.printStackTrace();
			}
		}
		try {
			dbConnection.commit();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public synchronized void clear() {
		try {
			stmt.execute("delete from LINKS");
			dbConnection.commit();
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
	}

	public void removeObject(IdentifiedObject lo) {
	}

	public synchronized void removeParent(Link link) {
		try {
			stmt.execute("delete from LINKS where CHILD_ID = '"
					+ link.getChild().getID() + "' AND PARENT_ID = '"
					+ link.getParent().getID() + "' AND TYPE_ID = '"
					+ link.getType().getID() + "'");
			dbConnection.commit();
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
	}

	public synchronized Collection<Link> getChildren(LinkedObject lo) {
		Collection<Link> out = new LinkedList<Link>();
		try {
			ResultSet rs = stmt
					.executeQuery("select TYPE_ID, CHILD_ID from LINKS where PARENT_ID = '"
							+ lo.getID() + "'");
			while (rs.next()) {
				String typeID = rs.getString("TYPE_ID");
				String childID = rs.getString("CHILD_ID");
				OBOProperty type = (OBOProperty) getObject(typeID);
				LinkedObject child = (LinkedObject) getObject(childID);
				if (type == null)
					System.err
							.println("** PULLED NULL TYPE FROM DATABASE ON ID "
									+ typeID);
				if (child == null)
					System.err
							.println("** PULLED NULL CHILD FROM DATABASE ON ID "
									+ childID);
				OBORestriction restriction = new OBORestrictionImpl(child,
						type, lo, true);
				out.add(restriction);
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
		// System.err.println("returning "+out+" for children of "+lo);
		return out;
	}

	public Collection<IdentifiedObject> getObjects() {
		// TODO Auto-generated method stub
		return null;
	}

	public synchronized Collection<Link> getParents(LinkedObject lo) {
		Collection<Link> out = new LinkedList<Link>();
		boolean failed = false;
		do {
			try {
				failed = false;
				ResultSet rs = stmt
						.executeQuery("select PARENT_ID, TYPE_ID from LINKS where CHILD_ID = '"
								+ lo.getID() + "'");
				if (rs == null) {
					failed = true;
				} else {
					while (rs != null && rs.next()) {
						String typeID = rs.getString("TYPE_ID");
						String parentID = rs.getString("PARENT_ID");
						OBOProperty type = (OBOProperty) getObject(typeID);
						LinkedObject parent = (LinkedObject) getObject(parentID);
						if (type == null)
							System.err
									.println("** PULLED NULL TYPE FROM DATABASE ON ID "
											+ typeID);
						if (parent == null)
							System.err
									.println("** PULLED NULL PARENT FROM DATABASE ON ID "
											+ parentID);
						out.add(new OBORestrictionImpl(lo, type, parent, true));
					}
				}
			} catch (SQLException ex) {
				failed = true;
			}
		} while (failed);
		// System.err.println("returning "+out+" for parents of "+lo);
		return out;
	}

	public IdentifiedObject getObject(String id) {
		if (index != null) {
			IdentifiedObject out = index.getObject(id);
			if (out == null) {
				System.err.println("$$$ GOT NULL LOOKUP ON " + id
						+ " FROM INDEX " + index);
			}
			return out;
		} else
			return null;
	}

	public static void main(String[] args) throws Exception {
		MutableLinkDatabase mldb = new SQLBackedMutableLinkDatabase(null);
		mldb.clear();
		mldb.addParent(new DanglingLinkImpl("GO:00001", "part_of", "GO:00005"));
		mldb.addParent(new DanglingLinkImpl("GO:00002", "part_of", "GO:00006"));
		mldb.addParent(new DanglingLinkImpl("GO:00003", "part_of", "GO:00007"));
		mldb.addParent(new DanglingLinkImpl("GO:00003", "part_of", "GO:00010"));
		mldb.addParent(new DanglingLinkImpl("GO:00004", "part_of", "GO:00008"));
		System.err.println("parents of GO:00003 = "
				+ mldb.getParents(new DanglingObjectImpl("GO:00003")));

	}

	public synchronized void clearParents(LinkedObject lo) {
		try {
			stmt.execute("delete from LINKS where CHILD_ID = '" + lo.getID()
					+ "'");
			dbConnection.commit();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
