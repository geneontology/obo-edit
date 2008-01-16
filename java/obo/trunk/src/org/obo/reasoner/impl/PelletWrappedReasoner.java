package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.bbop.util.TinySet;
import org.mindswap.pellet.KnowledgeBase;
import org.mindswap.pellet.jena.OWLReasoner;
import org.mindswap.pellet.taxonomy.Taxonomy;
import org.mindswap.pellet.utils.ATermUtils;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MutableLinkDatabase;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.util.TermUtil;

import aterm.ATerm;
import aterm.ATermAppl;
import aterm.ATermList;

public class PelletWrappedReasoner extends AbstractReasoner {

	private static final long serialVersionUID = -1L;

	protected OWLReasoner		owlReasoner;
	
	// should be protected; public for test purposes...
	public KnowledgeBase 	kb;

	// Pellet wrapper for knowledgebase
	protected Taxonomy 			tax;
	
	protected Map<String, ATermAppl> idToATerm = 
			new HashMap<String, ATermAppl>();
	protected Map<ATermAppl, IdentifiedObject> aTermToObject = 
			new HashMap<ATermAppl, IdentifiedObject>();
	protected Map<IdentifiedObject, Collection<ATerm>> objectToEquivATermSet = 
		new HashMap<IdentifiedObject, Collection<ATerm>>();

	protected Map<ATermAppl,Link> namedRestrToLink =
		new HashMap<ATermAppl,Link>();
	
	// pellet results are copied here
	protected MutableLinkDatabase impliedLinkDatabase;
	

	
	// TODO: protected
	// returns an ATermAppl object made from an OBO object
	// uses cache
	public ATermAppl makeATerm(IdentifiedObject lo) {
		String id = lo.getID();
		ATermAppl aterm = idToATerm.get(id);
		if (aterm != null) {
			return aterm;
		}
		else {
			System.err.println("adding aterm "+id+" lo="+lo+" type="+lo.getType());
			aterm = ATermUtils.makeTermAppl(id);
			idToATerm.put(id,aterm);
			aTermToObject.put(aterm,lo);
			if (TermUtil.isClass(lo)) {
				kb.addClass(aterm);
			}
			else if (TermUtil.isProperty(lo)) {
				kb.addObjectProperty(aterm);
				if (((OBOProperty)lo).isTransitive()) {
					kb.addTransitiveProperty(aterm);
				}
				if (((OBOProperty)lo).isSymmetric()) {
					kb.addSymmetricProperty(aterm);
				}
				if (((OBOProperty)lo).getTransitiveOver() != null) {
					Collection<ATerm> chain = new ArrayList<ATerm>();
					chain.add(aterm);
					chain.add(makeATerm(((OBOProperty)lo).getTransitiveOver()));
					// TODO
//					ATermAppl chainTerm = ATermUtils.makeList(chain);
//					kb.addSubProperty(aterm, chainTerm); 
				}
				else {
					//kb.addObjectProperty(aterm);
				}
			}
			else {
				kb.addIndividual(aterm);
			}
			return aterm;
		}
	}
	
	protected IdentifiedObject objectFromATerm(ATermAppl a) {
		return aTermToObject.get(a);
	}
	
	public void addLink(Link link) {
		LinkedObject child = link.getChild();
		LinkedObject parent = link.getParent();
		LinkedObject type = link.getType();
		ATermAppl c = makeATerm(child);
		ATermAppl p = makeATerm(parent);
		
		if (link instanceof OBORestriction) {
			OBORestriction rlink = (OBORestriction)link;
			if (rlink.getType().equals(OBOProperty.DISJOINT_FROM)) {
				kb.addDisjointClass(c, p);
				return;
			}
			if (rlink.getType().equals(OBOProperty.INVERSE_OF)) {
				kb.addInverseProperty(c, p);
				return;
			}
			if (rlink.completes()) {
				// TODO
				// collect and make OWL Restriction later...
				//ATermList ecterms = objectToEquivATermList.get(child);
				Collection<ATerm> ecterms = objectToEquivATermSet.get(child);
				if (ecterms == null) {
					//ATerm[] el = {};
					//ecterms = ATermUtils.makeList(el);
					ecterms = new LinkedList<ATerm>();
					objectToEquivATermSet.put(child,ecterms);
					System.out.println("saving EC for later: "+child+" "+ecterms);
				}
				if (type.equals(OBOProperty.IS_A)) {
					System.out.println("  adding genus "+c+" < "+p);
					ecterms.add(p);
				}
				else {
					ATermAppl t = makeATerm(type);
					ATermAppl restr = ATermUtils.makeSomeValues(t,p);
					System.out.println("  adding EC restriction "+c+" < "+restr+" /"+p);
					ecterms.add(restr);
				}
				System.out.println("ecterms="+ecterms.toString());
			}
			else {
				if (type.equals(OBOProperty.IS_A)) {
					if (link.getChild() instanceof OBOProperty) {
						System.err.println("adding subprop "+c+" < "+p);
						kb.addSubProperty(c, p);
					}
					else {
						System.err.println("adding subclass "+c+" < "+p);
						kb.addSubClass(c,p);
					}
				}
				else {
					ATermAppl t = makeATerm(type);
					if (link.getChild() instanceof OBOProperty) {
						// TODO
						System.err.println("ignoring "+link);
					}
					else {
						ATermAppl restr = ATermUtils.makeSomeValues(t,p);
						System.err.println("adding restriction "+c+" < "+restr+" /"+p);
						kb.addSubClass(c,restr);
					}
				}
			}
		}
	}

	public void addProperty(OBOProperty p) {
		if (p.equals(OBOProperty.IS_A)) {
			return;
		}
		System.out.println("adding prop "+p);
		ATermAppl atermProp = makeATerm(p);
		if (p.isTransitive()) {
			System.out.println("setting to transitive:" +atermProp);
			ATermUtils.makeTransitive(atermProp);
		}
		kb.addProperty(atermProp);
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		ATermAppl aterm = makeATerm(lo);
		
		// in OWL, the only class-level relation is subClassOf
		// non-is_a links are obtained by finding subclasses of
		// restrictions in which this class is the subject of quantification
		Set<Set<ATermAppl>> equivClassSetSet = tax.getSubs(aterm, false);
		TinySet<Link> s = new TinySet<Link>();
		for (Set<ATermAppl> ecs : equivClassSetSet) {
			for (ATermAppl xATerm : ecs) {
				if (ATermUtils.isBottom(xATerm)) {
					System.out.println("ignoring BOTTOM");
				}
				else {
					LinkedObject x = (LinkedObject)objectFromATerm(xATerm);
					Link newRel = findOrCreateLink(x, OBOProperty.IS_A,
							lo);
					s.add(newRel);
				}
			}		
		}
		// TODO: Restrictions
		return s;
	}
	
	// does not cache links. Is this a problem?
	protected Link findOrCreateLink(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		Link out = new OBORestrictionImpl(child, type, parent, true);
		return out;
	}

	

	public Collection<OBOProperty> getSuperProperties(OBOProperty p) {
		ATermAppl aterm = makeATerm(p);
		
		// in OWL, the only class-level relation is subClassOf
		// non-is_a links are obtained by finding the superclasses that are restrictions
		Set<Set<ATermAppl>> equivPropSetSet = kb.getSuperProperties(aterm, false);
		Collection<OBOProperty> s = new TinySet<OBOProperty>();
		for (Set<ATermAppl> ecs : equivPropSetSet) {
			for (ATermAppl parentATerm : ecs) {
				if (ATermUtils.isTop(parentATerm)) {
					System.out.println("ignoring TOP");
				}
				else {
					// casting alert...
					OBOProperty parent = (OBOProperty)objectFromATerm(parentATerm);
					s.add(parent);
				}
			}		
		}
		return s;
	}

	public Collection<Link> getParents(LinkedObject lo) {
		ATermAppl aterm = makeATerm(lo);
		
		System.out.println(" getting parents of "+lo.getID()+" "+lo);
		// in OWL, the only class-level relation is subClassOf
		// non-is_a links are obtained by finding the superclasses that are restrictions
		Set<Set<ATermAppl>> equivClassSetSet = tax.getSupers(aterm, false);
		Collection<Link> s = new TinySet<Link>();
		for (Set<ATermAppl> ecs : equivClassSetSet) {
			for (ATermAppl parentATerm : ecs) {
				System.out.println("    p="+parentATerm);
				if (ATermUtils.isTop(parentATerm)) {
					System.out.println("ignoring TOP");
				}
//				else if (ATermUtils.isSomeValues(parentATerm)) {
//				}
				else {
					// previously constructed hashset mapping
					// a named class (not in original ontology) and
					// a parent link. Each of these named classes
					// has been declared equivalent to an anonymous
					// restriction 
					Link plink = namedRestrToLink.get(parentATerm);
					if (plink == null) {
						LinkedObject parent = (LinkedObject)objectFromATerm(parentATerm);
						Link newRel = findOrCreateLink(lo, OBOProperty.IS_A,
								parent);
						s.add(newRel);
					}
					else {
						s.add(findOrCreateLink(lo, plink.getType(), plink.getParent()));
					}
				}
			}		
		}
		return s;
	}
	
	public Link hasRelationship(LinkedObject a, OBOProperty b, LinkedObject c) {
		
		if (b.isBuiltIn())
			for (Link link : a.getParents())
				if (link.getType().equals(b) && link.getParent().equals(c))
					return link;
		
		return super.hasRelationship(a, b, c);
	}






	public boolean isInstanceOf(Instance a, OBOClass b) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isRedundant(Link link) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isSubPropertyOf(OBOProperty a, OBOProperty b) {
		if (a.equals(b)) {
			return true;
		}
		for (OBOProperty p : getSuperProperties(a)) {
			if (p.getID().equals(b.getID())) {
				return true;
			}
		}
		return false;
	}

	public boolean isSubclassOf(OBOClass a, OBOClass b) {
		// TODO Auto-generated method stub
		return false;
	}
	
	public void removeLink(Link link) {
		// TODO Auto-generated method stub

	}
	
	protected void setEquivalentClasses() {
		for (IdentifiedObject lo : objectToEquivATermSet.keySet()) {
			String id = lo.getID();
			ATermAppl aterm = idToATerm.get(id);
			Collection<ATerm> ecaterms = objectToEquivATermSet.get(lo);
			ATermList tl = ATermUtils.makeList(ecaterms);
			System.out.println("tl="+tl);
			ATermAppl ec = ATermUtils.makeAnd(tl);
			kb.addEquivalentClass(aterm, ec);
			System.out.println("EC "+aterm+","+ec);
		}
	}

	public long recache() {
		
		System.out.println("recaching. getting OWL reasoner...");
		owlReasoner = new OWLReasoner();
		System.out.println("set OWL reasoner =   "+owlReasoner);
		kb = owlReasoner.getKB();
		System.out.println("kb = "+kb);
		
		idToATerm = 
			new HashMap<String, ATermAppl>();
		aTermToObject = 
			new HashMap<ATermAppl, IdentifiedObject>();
		objectToEquivATermSet = 
			new HashMap<IdentifiedObject, Collection<ATerm>>();

		namedRestrToLink =
			new HashMap<ATermAppl,Link>();
		
		Collection<OBOProperty> props = new HashSet<OBOProperty>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof LinkedObject) {
				for (Link link : linkDatabase.getParents((LinkedObject) io)) {
					addLink(link);
				}
				
			}
			if (io instanceof OBOProperty) {
				addProperty((OBOProperty)io);
				props.add((OBOProperty)io);
			}
		}
		
		// needed for TBox reasoning
		// for every P x Class we make a named class "PC" and declare it
		// equivalentTo Restriction(P C)
		for (OBOProperty prop : props) {
			for (IdentifiedObject io : linkDatabase.getObjects()) {
				if (io.isBuiltIn())
					continue;
				if (io instanceof LinkedObject) {
					ATermAppl t = makeATerm(prop);
					ATermAppl p = makeATerm(io);
					ATermAppl namedRestr = ATermUtils.makeTermAppl(prop.getID()+"---"+io.getID());
					ATermAppl fakeChild = makeATerm(io);
					ATermAppl restr = ATermUtils.makeSomeValues(t,p);
					kb.addEquivalentClass(namedRestr, restr);
					kb.addClass(namedRestr);
					namedRestrToLink.put(namedRestr, 
							findOrCreateLink(null,prop,(LinkedObject)io));
				}
			}
			
		}
		setEquivalentClasses();

		owlReasoner.classify();
		owlReasoner.realize();
		tax = kb.getTaxonomy();
		//copyToImpledLinkDatabase();
		return 0;
	}
		
	public Collection<Explanation> getExplanations(PathCapable pc) {
		return null;
	}

	public boolean hasChildren(LinkedObject lo) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean hasParents(LinkedObject lo) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	protected void doAddLink(Link link) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void doReasoning() {
		// TODO Auto-generated method stub
		
	}


}
