package org.blipkit.reasoner.impl;

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


import org.obo.datamodel.CategorizedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.IdentifiedObjectIndex;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MultiIDObject;
import org.obo.datamodel.MutableLinkDatabase;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.ObjectFactory;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.TermCategory;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.reasoner.impl.AbstractReasoner;
import org.obo.util.TermUtil;

public class DatalogReasoner extends AbstractReasoner {

	protected String dbName = "datalog_cache.pro";
	
	protected ObjectFactory objectFactory = new DefaultObjectFactory();

	protected IdentifiedObjectIndex index;
	
	public DatalogReasoner() {
		super();
		new Query("use_module(bio(metadata_db))").allSolutions();
		new Query("use_module(bio(ontol_db))").allSolutions();
		new Query("use_module(bio(ontol_reasoner))").allSolutions();
	}

	

	public void addObject(IdentifiedObject lo) {
		String pred = "inst";
		String id = lo.getID();
		
		if (lo.isBuiltIn())
			return;
		
		if (lo instanceof OBOClass) {
			pred = "class";
		}
		else if (lo instanceof OBOProperty) {
			pred = "property";
			OBOProperty prop = (OBOProperty)lo;
			if (prop.isTransitive())
				assertOntolFact("is_transitive",id);
			if (prop.isSymmetric())
				assertOntolFact("is_symmetric",id);
			if (prop.getDomain() != null)
				assertOntolFact("domain",id,prop.getDomain().getID());
			if (prop.getRange() != null)
				assertOntolFact("range",id,prop.getRange().getID());
			if (prop.isNonInheritable())
				assertOntolFact("is_metadata_tag", id);
		}
		else {
			pred = "inst";
			Instance inst = (Instance)lo;
			if (inst.getType() != null) {
				assertOntolFact("inst_of",id,inst.getType().getID());
			}
		}
		if (lo instanceof ObsoletableObject && ((ObsoletableObject)lo).isObsolete()) {
			assertMetadataFact("entity_obsolete",id,pred);
			for (IdentifiedObject x : ((ObsoletableObject)lo).getConsiderReplacements())
				assertMetadataFact("entity_consider",id,x);
			for (IdentifiedObject x : ((ObsoletableObject)lo).getReplacedBy())
				assertMetadataFact("entity_replaced_by",id,x);
			
		}
		else {
			assertOntolFact(pred,id);
		}
		assertMetadataFact("entity_label",id,lo.getName());
		
		if (lo instanceof DbxrefedObject) {
			for (Dbxref x : ((DbxrefedObject) lo).getDbxrefs()) {
				assertMetadataFact("entity_dbxref",id,x.toString());
			}
		}
		if (lo instanceof CategorizedObject) {
			for (TermCategory x : ((CategorizedObject) lo).getCategories()) {
				assertMetadataFact("entity_partition",id,x.getName());
			}
		}
		if (lo instanceof MultiIDObject) {
			for (String x : ((MultiIDObject) lo).getSecondaryIDs()) {
				assertMetadataFact("entity_xref",id,x);
			}
		}
		if (lo instanceof SynonymedObject) {
			for (Synonym x : ((SynonymedObject) lo).getSynonyms()) {
				assertMetadataFact("entity_synonym",
						id,x.getText());
				assertMetadataFact("entity_synonym_scope",
						id,TermUtil.getScopeLabel(x.getScope()),x.getText());
				if (x.getSynonymCategory() != null)
					assertMetadataFact("entity_synonym_type",
							id,x.getSynonymCategory(),x.getText());
			}
		}
		if (lo instanceof DefinedObject) {
			String def = ((DefinedObject) lo).getDefinition();
			if (def != null) {
				assertOntolFact("def",
						id,def);
				
			}
		}
		Namespace ns = lo.getNamespace();
		if (ns != null) {
			assertMetadataFact("entity_resource",id,ns.getID());				
		}
		if (lo.isAnonymous())
			assertOntolFact("is_anonymous", id);
		
	}
	


	public void addParents(Collection<Link> links) {
		
		for (Link link : links) {

			LinkedObject child = link.getChild();
			LinkedObject parent = link.getParent();
			OBOProperty type = link.getType();
			if (link instanceof OBORestriction)
				if (type.equals(OBOProperty.IS_A))
					if (TermUtil.isIntersection(link))
						assertOntolFact("genus",child.getID(),parent.getID());
					else
						assertOntolFact("subclass",child.getID(),parent.getID());
				else if (type.equals(OBOProperty.DISJOINT_FROM))
					assertOntolFact("disjoint_from",child.getID(),parent.getID());
				else if (TermUtil.isIntersection(link))
					assertOntolFact("differentium",child.getID(),type.getID(),parent.getID());
				else
					assertOntolFact("restriction",child.getID(),type.getID(),parent.getID());
			else
				assertOntolFact("inst_rel",child.getID(),type.getID(),parent.getID());
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

	public synchronized Collection<Link> getParents(LinkedObject lo) {
		Collection<Link> out = new LinkedList<Link>();
	
		out.addAll(getSuperclasses(lo));

		Variable R = new Variable("R");
		Variable Y = new Variable("Y");
		Query q = 
			makeQuery("ontol_db","restriction",new Term[]{new Atom(lo.getID()), R, Y});
		for (Hashtable h : q.allSolutions()) {
			Link link;
			OBOProperty prop = (OBOProperty)getObject(((Atom)h.get("R")).name());
			LinkedObject parent = (LinkedObject) getObject(((Atom)h.get("Y")).name());
			System.out.println("sc: restr"+prop+" "+parent);
			link = objectFactory.createOBORestriction(lo, prop, parent, false);
			out.add(link);
		}
		// System.err.println("returning "+out+" for parents of "+lo);
		return out;
	}
	
	public synchronized Collection<Link> getSuperclasses(LinkedObject lo) {
		Collection<Link> out = new LinkedList<Link>();
	
		Variable Y = new Variable("Y");
		Query q = 
			makeQuery("ontol_db","subclass",new Term[]{new Atom(lo.getID()), Y});

		for (Hashtable h : q.allSolutions()) {
			Atom pidt = (Atom)h.get("Y");
			String parentId = pidt.name();
			System.out.println("sc:"+parentId);
			Link link;
			//System.err.println("n-objs: "+linkDatabase.getObjects().size());
			LinkedObject parent = (LinkedObject) getObject(parentId);
			link = objectFactory.createOBORestriction(lo, OBOProperty.IS_A, parent, false);
			out.add(link);
		}
		
		// System.err.println("returning "+out+" for parents of "+lo);
		return out;
	}
	
	public Query makeQuery(String mod, String pred, Term[] termA) {
		return new Query(":",new Term[]{new Atom(mod),new Compound(pred,termA)});
	}



	public synchronized void clearParents(LinkedObject lo) {
		retractAll("subclass",lo.getID(),"_");
		retractAll("restriction",lo.getID(),"_","_");
		
	}
	
	protected int assertOntolFact(String pred, Object... args) {
		return assertFact("ontol_db",pred,args);
	}	
	protected int assertMetadataFact(String pred, Object... args) {
		return assertFact("metadata_db",pred,args);
	}
	
	protected int assertFact(String mod, String pred, Object... args) {
		System.err.println("asserting "+pred);
		Atom[] atomA = new Atom[args.length];
		int i=0;
		for (Object a: args) {
			System.err.println("  ::"+a);
			atomA[i] = new Atom((String)a);
			i++;
		}
		Compound iterm = new Compound(pred,atomA);
		
		Compound term = new Compound(":",new Term[]{new Atom(mod),iterm});
		Compound assertTerm = new Compound("assert",new Term[]{term});
		new Query(assertTerm).allSolutions();
		System.err.println("asserted "+assertTerm);
		return 0;
	}
	
	protected int retractAll(String pred, Object... args) {
		Compound term = new Compound(pred,(Atom[]) args);
		Compound rTerm = new Compound("retractall",new Term[]{term});
		new Query(rTerm);
		return 0;
	}

	@Override
	protected void doAddLink(Link link) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void doReasoning() {
		System.out.println("recaching. adding to datalog database");
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof LinkedObject) {
				addObject(io);
				for (Link link : linkDatabase.getParents((LinkedObject) io)) {
					addParent(link);
				}
			}
			if (io instanceof OBOProperty) {
//				addProperty((OBOProperty)io);
			}
		}
		new Query("find_all_entailments").allSolutions();
		//copyToImpledLinkDatabase();
		
	}
	 


}
