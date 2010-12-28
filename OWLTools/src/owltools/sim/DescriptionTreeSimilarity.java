package owltools.sim;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLPropertyRange;
import org.semanticweb.owlapi.model.OWLQuantifiedRestriction;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;
import owltools.sim.SimEngine.SimilarityAlgorithmException;

/**
 * A similarity between two entities constructed by threading two description trees together.
 * 
 * ALGORITHM:
 * 
 * <pre>
 * <code>
 * build(a,b) :
 *   N = {}
 *   foreach direct parent edge Ea:
 *     find closest match for Ea.tgt in all reflexive ancestors of b
 *     Eb = edge between b and b'
 *     LCS = all least common reachable ancestors of a,b
 *     if |LCS| = 0 : n = null
 *              = 1 : n = < LCS[1] Ea Eb>
 *              > 1 : n = build(Ea.t,Eb.t)
 *                    extend n.Ea + Ea
 *                    extent n.Eb + Eb
 *      add n to N
 *      
 *   N' = map { pathToExpression } N
 *   if |N| = 0 : return null
 *          = 1 : return N'[1]
 *          > 1 : return IntersectionOf(N')
 *  </code>
 *  </pre>
 *   
 *   
 * @author cjm
 *
 */
public class DescriptionTreeSimilarity extends Similarity {
	private static Logger LOG = Logger.getLogger(DescriptionTreeSimilarity.class);

	OWLClassExpression lcs;
	OWLGraphWrapper graph;
	SimEngine se;
	// move to config?
	public boolean forceReflexivePropertyCreation = true;
	Set<OWLObject> visited = new HashSet<OWLObject>();

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) throws SimilarityAlgorithmException {
		se = simEngine;
		graph = simEngine.getGraph();
		ConvergentPath cp = buildDescription(a,b);
		lcs = combinePathsToMakeExpression(cp);
		score = 1.0; // TODO
	}

	private class ObjectPair {
		private final OWLObject a;
		private final OWLObject b;
		private transient final int hash;

		public ObjectPair(OWLObject a, OWLObject b) {
			super();
			this.a = a;
			this.b = b;
			hash = (a == null? 0 : a.hashCode() * 31)+(b == null? 0 : b.hashCode());
		}
		@Override
		public int hashCode()
		{
			return hash;
		}
		public boolean equals(Object x) {
			if (!(x instanceof ObjectPair))
				return false;
			return ((ObjectPair)x).getA().equals(a) &&
			((ObjectPair)x).getA().equals(a);

		}
		public OWLObject getA() {
			return a;
		}
		public OWLObject getB() {
			return b;
		} 


	}

	private class ConvergentPath {
		OWLClassExpression x;
		OWLGraphEdge ea;
		OWLGraphEdge eb;

		public String toString() {
			return x.toString()+" EA:"+ea+" EB:"+eb;
		}
	}

	/**
	 * Recursively build a class expression that subsumes a and b.
	 * 
	 * Algorithm: traverse description tree a, one edge at a time, finding the best subtree in b that
	 * matches. if there are multiple paths from a leading to a LCS, create an intersection expression.
	 */
	public ConvergentPath buildDescription(OWLObject a, OWLObject b) {
		Set<OWLObject> fullLCSs = se.getLeastCommonSubsumers(a,b);
		System.out.println("orig LCSs="+fullLCSs);
		return buildDescription(a,b,fullLCSs);
	}

	public ConvergentPath buildDescription(OWLObject a, OWLObject b, Set<OWLObject> fullLCSs) {

		if (visited.contains(a))
			return null;

		visited.add(a);

		System.out.println("building:"+a+" vs "+b);
		// candidate expression
		OWLClassExpression x = null;

		ConvergentPath cp = new ConvergentPath();

		// convergence
		if (a.equals(b)) {
			System.out.println("   IDENTICAL");
			cp.x = (OWLClassExpression) a;
			return cp;
			//return combinePathsToMakeExpression(a, ea, eb);
		}

		// iterate through all edges leading from a
		// TODO - only extend when no intersection is built
		Set<OWLClassExpression> subExprs = new HashSet<OWLClassExpression>();
		Set<ConvergentPath> subCps = new HashSet<ConvergentPath>();
		Set<OWLGraphEdge> aEdges = graph.getOutgoingEdges(a);
		for (OWLGraphEdge e : aEdges) {
			System.out.println("  edge: "+e);
			//OWLGraphEdge eaNew = graph.combineEdgePair(ea.getSource(), ea, e, 1);
			OWLObject aNext = e.getTarget();


			// get best match for a in {b, desc-of-b}
			// also - restrict subsumers
			// TODO
			OWLObject bNext = b;
			double best = 0;
			for (OWLObject candidate : graph.getAncestorsReflexive(b)) {
				// TODO - optimize - we only need to calculate aNext's subsumers once
				Set<OWLObject> csl = se.getCommonSubsumers(aNext, candidate);
				Set<OWLObject> usl = se.getUnionSubsumers(aNext, candidate);
				double score = (double) csl.size() / usl.size();
				//System.out.println("    candidate: "+candidate+" score:"+score);
				if (score > best) {
					best = score;
					bNext = candidate;
				}
			}
			if (best == 0)
				continue;
			System.out.println("  bNext: "+bNext);

			Set<OWLObject> lcsl = se.getCommonSubsumers(aNext, bNext);
			System.out.println("    lcsl: "+lcsl);
			lcsl.retainAll(fullLCSs);
			System.out.println("    lcsl, full: "+lcsl);

			if (lcsl.size() == 0)
				continue;

			// what do we do about multiple edges?
			// ideally choose the one closest to ea.
			// for now choose arbitrary one
			Set<OWLGraphEdge> bEdges = graph.getEdgesBetween(b, bNext);
			System.out.println("    bEdges: "+bEdges);
			OWLGraphEdge bEdge = bEdges.iterator().next();
			//OWLGraphEdge ebNew = graph.combineEdgePair(eb.getSource(), eb,
			//		bEdges.iterator().next(), 1);

			// TODO - reset edges if we make an intersection below
			OWLClassExpression nextExpr;
			ConvergentPath nextCp;
			if (lcsl.size() == 1) {
				// convergence; no need to recurse further
				nextCp = new ConvergentPath();
				nextCp.x = (OWLClassExpression) lcsl.iterator().next();
				OWLGraphEdge extEdge = graph.getEdgesBetween(aNext, nextCp.x).iterator().next();
				nextCp.ea = graph.combineEdgePair(e.getSource(), e, extEdge, 1);
				nextCp.eb = bEdge;
				System.out.println(" CONVERGE:"+nextCp.x+" combine:"+e+" * "+extEdge+" = "+nextCp.ea);
				//nextExpr = combinePathsToMakeExpression(lcsl.iterator().next(),e,bEdge);
			}
			else {
				nextCp = buildDescription(aNext, bNext, lcsl);
				if (nextCp == null)
					continue;

				if (nextCp.ea == null)
					nextCp.ea = e;
				else 
					nextCp.ea = graph.combineEdgePair(a, e, nextCp.ea, 1);
				if (nextCp.eb == null)
					nextCp.eb = bEdge;
				else 
					nextCp.eb = graph.combineEdgePair(b, bEdge, nextCp.eb, 1); // TODO
			}

			subCps.add(nextCp);
			//subExprs.add(nextExpr);
		}

		if (subCps.size() == 0) {
			return null;

		}
		else if (subCps.size() == 1) {
			cp = subCps.iterator().next();

			// edges must be extended here
			//x = subExprs.iterator().next();
		}
		else {
			// TODO - edges combined and reset here
			subExprs = new HashSet<OWLClassExpression>();
			System.out.println("combining sub-expressions...");
			for (ConvergentPath subCp : subCps) {
				System.out.println("  sub-CP:"+subCp);
				subExprs.add(combinePathsToMakeExpression(subCp));
			}
			cp.x = graph.getDataFactory().getOWLObjectIntersectionOf(subExprs);
			cp.ea = null;
			cp.eb = null;
		}

		return cp;
	}

	private OWLClassExpression combinePathsToMakeExpression(ConvergentPath cp) {
		// todo - null edges - treat as identity
		return combinePathsToMakeExpression(cp.x, cp.ea, cp.eb);
	}

	/**
	 * given two paths to the same node..
	 * TODO - also include from tips of edges to LCS..?
	 * 
	 * @param x
	 * @param ea
	 * @param eb
	 * @return
	 */
	private OWLClassExpression combinePathsToMakeExpression(OWLObject x, OWLGraphEdge ea, OWLGraphEdge eb) {
		System.out.println("combining, tgt="+x+" EA="+ea+" EB="+eb);
		Set<OWLClassExpression> args = new HashSet<OWLClassExpression>();
		if (ea == null) {
			ea = new OWLGraphEdge(null,x,new Vector<OWLQuantifiedProperty>(),null);
		}
		if (eb == null) {
			eb = new OWLGraphEdge(null,x,new Vector<OWLQuantifiedProperty>(),null);
		}
		System.out.println("making Union of "+ea+" and "+eb);
		ea.setTarget(x);
		eb.setTarget(x);
		return makeUnion3(ea,eb);
		/*
		OWLClassExpression xa;
		OWLClassExpression xb;
		if (ea == null)
			xa = (OWLClassExpression) x;
		else {
			ea.setTarget(x);
			xa = (OWLClassExpression) graph.edgeToTargetExpression(ea);
		}
		if (eb == null)
			xb = (OWLClassExpression) x;
		else {
			eb.setTarget(x);
			xb = (OWLClassExpression) graph.edgeToTargetExpression(eb);
		}
		System.out.println("  XA="+xa+" XB="+xb);
		boolean ia = xa.equals(x);
		boolean ib = xb.equals(x);
		if (ia && ib) {
			return xa;
		}
		else if (ia) {
			return makeUnion2(x,xb);
			//return makeUnion(x, eb);
		}
		else if (ib) {
			return makeUnion2(x,xa);
			//return makeUnion(x, ea);
		}
		else {
			return makeUnion(xa, xb);			
		}
		 */
	}

	private OWLClassExpression makeUnion3(OWLGraphEdge ea, OWLGraphEdge eb) {
		OWLClassExpression xa = (OWLClassExpression)graph.edgeToTargetExpression(ea);
		OWLClassExpression xb = (OWLClassExpression)graph.edgeToTargetExpression(eb);
		return makeUnionUsingClever(xa,xb);
	}

	private OWLClassExpression makeUnionUsingClever(OWLClassExpression xa, OWLClassExpression xb) {
		OWLClassExpression rx = makeUnionUsingReflexiveProperty(xa,xb);
		if (rx == null)
			rx = makeUnionUsingReflexiveProperty(xb,xa);
		if (rx == null) {
			rx = makeUnion(xa,xb);
		}
		return rx;		
	}
	
	private OWLObjectPropertyExpression propertySubsumer(OWLObjectPropertyExpression p1, OWLObjectPropertyExpression p2) {
		if (p1.equals(p2))
			return p1;
		// TODO - check for topObjectProp?
		if (graph.getSuperPropertiesOf(p1).contains(p2)) {
			return p2;
		}
		if (graph.getSuperPropertiesOf(p2).contains(p1)) {
			return p1;
		}
		return null;
	}

	// todo: (has some sn) OR (has some part_of some sn) 
	private OWLClassExpression makeUnionUsingReflexiveProperty(OWLClassExpression xa, OWLClassExpression xb) {
		System.out.println("testing if there is a more compact union expression for "+xa+" ++ "+xb);
		OWLDataFactory df = graph.getDataFactory();
		if (xa instanceof OWLQuantifiedRestriction) {
			// TODO - check before casting
			OWLObjectProperty prop = (OWLObjectProperty) ((OWLQuantifiedRestriction) xa).getProperty();
			OWLClassExpression xaRest = (OWLClassExpression) ((OWLQuantifiedRestriction)xa).getFiller();
			if (xb instanceof OWLQuantifiedRestriction) {
				OWLObjectPropertyExpression p2 = propertySubsumer(prop, 
						((OWLQuantifiedRestriction<OWLObjectPropertyExpression, OWLClassExpression>) xb).getProperty());
				
				if (p2 != null) {
					OWLClassExpression xbRest = (OWLClassExpression) ((OWLQuantifiedRestriction)xb).getFiller();
					OWLClassExpression x = makeUnionUsingClever(xaRest,xbRest);
					// todo - mixing some and all
					if (xa instanceof OWLObjectSomeValuesFrom)
						return df.getOWLObjectSomeValuesFrom(p2,x);
					else if (xa instanceof OWLObjectAllValuesFrom)
						return df.getOWLObjectAllValuesFrom(p2, x);
				}
			}
			System.out.println("  test: "+xaRest+" == "+xb);
			if (xaRest.equals(xb)) {
				System.out.println("     TRUE: "+xaRest+" == "+xb);

				OWLObjectProperty rprop = null;
				if (graph.getIsReflexive(prop)) {
					rprop = prop;
				}
				if (forceReflexivePropertyCreation) {
					OWLOntologyManager manager = graph.getManager();
					OWLOntology ont = graph.getSourceOntology();
					rprop = 
						df.getOWLObjectProperty(IRI.create(prop.getIRI().toString()+"_reflexive"));
					manager.applyChange(new AddAxiom(ont, df.getOWLSubObjectPropertyOfAxiom(prop, rprop)));
					manager.applyChange(new AddAxiom(ont, df.getOWLTransitiveObjectPropertyAxiom(rprop)));
					System.out.println("  reflexive prop:"+rprop);

				}
				if (rprop != null) {
					if (xa instanceof OWLObjectSomeValuesFrom)
						return df.getOWLObjectSomeValuesFrom(rprop,xb);
					else if (xa instanceof OWLObjectAllValuesFrom)
						return df.getOWLObjectAllValuesFrom(rprop, xb);
				}

			}
		}
		return null;
	}


	@Deprecated
	private OWLClassExpression makeUnion2(OWLObject x, OWLClassExpression rx) {
		System.out.println("Union: "+x+" OR "+rx);
		if (rx instanceof OWLQuantifiedRestriction) {
			OWLQuantifiedRestriction qx = (OWLQuantifiedRestriction)rx;
			if (qx.getFiller().equals(x)) {
				OWLObjectProperty prop = (OWLObjectProperty) qx.getProperty();
				OWLObjectProperty rprop = null;
				OWLDataFactory df = graph.getDataFactory();
				if (graph.getIsReflexive(prop)) {
					rprop = prop;
				}
				if (forceReflexivePropertyCreation) {
					OWLOntologyManager manager = graph.getManager();
					OWLOntology ont = graph.getSourceOntology();
					rprop = 
						df.getOWLObjectProperty(IRI.create(prop.getIRI().toString()+"_reflexive"));
					manager.applyChange(new AddAxiom(ont, df.getOWLSubObjectPropertyOfAxiom(prop, rprop)));
					manager.applyChange(new AddAxiom(ont, df.getOWLTransitiveObjectPropertyAxiom(rprop)));
					System.out.println("  reflexive prop:"+rprop);

				}
				if (rprop != null) {
					if (qx instanceof OWLObjectSomeValuesFrom)
						return df.getOWLObjectSomeValuesFrom(rprop, (OWLClassExpression) x);
					else if (qx instanceof OWLObjectAllValuesFrom)
						return df.getOWLObjectAllValuesFrom(rprop, (OWLClassExpression) x);
				}
			}
		}
		return makeUnion((OWLClassExpression)x, rx);
	}

	private OWLClassExpression makeUnion(OWLClassExpression xa, OWLClassExpression xb) {
		if (xa.equals(xb)) {
			return xa;
		}
		else {
			Set<OWLClassExpression> args = new HashSet<OWLClassExpression>();
			args.add(xa);
			args.add(xb);
			return graph.getDataFactory().getOWLObjectUnionOf(args);
		}
	}

	public void print(PrintStream s) {
		s.println("LCS:");
		printX(s,lcs);
	}

	public void printX(PrintStream s, OWLObject x) {
		printX(s,x,0);
		s.print("\n");
	}


	public void printX(PrintStream s, OWLObject x, int depth) {
		tab(s,depth);
		if (x instanceof OWLObjectIntersectionOf) {
			s.print("and");
			for (OWLClassExpression sx : ((OWLObjectIntersectionOf)x).getOperands()) {
				printX(s, sx, depth+1);
			}
		}
		else if (x instanceof OWLObjectUnionOf) {
			s.print("or");
			for (OWLClassExpression sx : ((OWLObjectUnionOf)x).getOperands()) {
				printX(s, sx, depth+1);
			}
		}
		else if (x instanceof OWLQuantifiedRestriction) {
			OWLQuantifiedRestriction qr = (OWLQuantifiedRestriction)x;
			s.print(qr.getProperty().toString());
			printX(s, qr.getFiller(), depth+1);
		}
		else if (x instanceof OWLNamedObject) {
			String label = graph.getLabel(x);
			if (label != null)
				s.print(x.toString()+" \""+label+"\"");
			else
				s.print(x);
		}
		else {
			s.print(x);
		}
	}

	private void tab(PrintStream s, int depth) {
		s.print("\n");
		for (int i=0; i<depth; i++)
			s.print("  ");

	}

}
