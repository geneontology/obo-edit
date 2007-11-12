package org.blipkit.datamodel.impl;

import java.io.File;
import java.sql.CallableStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.Properties;
import jpl.*;


import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.IdentifiedObjectIndex;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MutableLinkDatabase;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.ObjectFactory;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.datamodel.impl.DefaultObjectFactory;

public class DatalogBackedMutableLinkDatabase extends AbstractLinkDatabase
		implements MutableLinkDatabase {

	protected String dbName = "datalog_cache.pro";
	
	protected ObjectFactory objectFactory;

	protected IdentifiedObjectIndex index;

	public DatalogBackedMutableLinkDatabase(IdentifiedObjectIndex index) {
		initializeDatabase();
		objectFactory = new DefaultObjectFactory();
		setIdentifiedObjectIndex(index);
	}

	public void setIdentifiedObjectIndex(IdentifiedObjectIndex index) {
		this.index = index;
	}

	protected void initializeDatabase() {
	}


	public void addObject(IdentifiedObject lo) {
		String pred = "inst";
		if (lo instanceof OBOClass) {
			pred = "class";
		}
		else if (lo instanceof OBOProperty) {
			pred = "property";
		}
		else {
			
		}
		assertFact(pred,lo.getID());
		assertFact("metadata_db:entity_label",lo.getID(),lo.getName());
		
	}
	


	public void addParents(Collection<Link> links) {
		
		for (Link link : links) {

			LinkedObject child = link.getChild();
			LinkedObject parent = link.getParent();
			OBOProperty type = link.getType();
			if (link instanceof OBORestriction)
				if (type.equals(OBOProperty.IS_A))
					assertFact("subclass",child.getID(),parent.getID());
				else
					assertFact("restriction",child.getID(),type.getID(),parent.getID());
			else
				assertFact("inst_rel",child.getID(),type.getID(),parent.getID());
		}
	}

	public synchronized void addParent(Link link) {
		addParents(Collections.singleton(link));
	}

	public synchronized void setParents(LinkedObject lo, Collection<Link> links) {
		retractAll("link",lo.getID(),getAnon(),getAnon());
		for (Link link : links) {
			//TODO
		}
	}
	
	protected Term getAnon() {
		return new Variable("_");
	}

	public synchronized void clear() {
		retractAll("link",getAnon(),getAnon(),getAnon());
	}

	public void removeObject(IdentifiedObject lo) {
	}

	public synchronized void removeParent(Link link) {
		//TODO
	}

	public synchronized Collection<Link> getChildren(LinkedObject lo) {
		Collection<Link> out = new LinkedList<Link>();
		//TODO
		
		// System.err.println("returning "+out+" for children of "+lo);
		return out;
	}

	public Collection<IdentifiedObject> getObjects() {
		// TODO Auto-generated method stub
		return null;
	}

	public synchronized Collection<Link> getParents(LinkedObject lo) {
		Collection<Link> out = new LinkedList<Link>();
		//TODO
		Variable R = new Variable("R");
		Variable Y = new Variable("Y");
		Query q = 
			new Query("node_link",new Term[]{new Atom(lo.getID()), R, Y});
		for (Hashtable h : q.allSolutions()) {
			System.out.println(h.get("X"));
			Link link;
			OBOProperty prop = (OBOProperty)getObject(R.toString());
			LinkedObject parent = (LinkedObject) getObject(Y.toString());
			link = objectFactory.createOBORestriction(lo, prop, parent, false);
			out.add(link);
		}
		
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



	public synchronized void clearParents(LinkedObject lo) {
		retractAll("subclass",lo.getID(),"_");
		retractAll("restriction",lo.getID(),"_","_");
		
	}
	
	protected int assertFact(String pred, Object... args) {
		Compound term = new Compound(pred,(Atom[]) args);
		Compound assertTerm = new Compound("assert",new Term[]{term});
		new Query(assertTerm);
		return 0;
	}
	
	protected int retractAll(String pred, Object... args) {
		Compound term = new Compound(pred,(Atom[]) args);
		Compound rTerm = new Compound("retractall",new Term[]{term});
		new Query(rTerm);
		return 0;
	}
	 


}
