package owltools.sim;

import java.util.HashMap;
import java.io.OutputStream;
import java.io.PrintStream;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLObject;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.sim.SimEngine.SimilarityAlgorithmException;

public class DescriptionTreeSimilarity extends Similarity {
	Map <OWLObject,Similarity> matchMap = new HashMap<OWLObject,Similarity>();
	Set <OWLObject> aset;
	Set <OWLObject> bset;

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) throws SimilarityAlgorithmException {
		OWLGraphWrapper graph = simEngine.getGraph();
		Map <OWLObject,Similarity> bestMap = new HashMap<OWLObject,Similarity>();
		aset = new HashSet<OWLObject>();
		bset = new HashSet<OWLObject>();

		for (OWLGraphEdge ea : graph.getOutgoingEdges(a)) {
			OWLObject pa = ea.getTarget();
			aset.add(pa);
			for (OWLGraphEdge eb : graph.getOutgoingEdges(b)) {
				OWLObject pb = eb.getTarget();
				bset.add(pb);
				//if (pa.compareTo(pb) < 0)
				//	continue;
				Set<OWLObject> lcss = simEngine.getLeastCommonSubsumers(pa, pb);
				Set<OWLObject> lcsxs = new HashSet<OWLObject>();
				for (OWLObject lcs : lcss) {
					OWLObject lcsx = simEngine.createUnionExpression(pa,pb,lcs);
					lcsxs.add(lcsx);
					System.out.println("  LCSX="+lcsx);
				}
				// todo - cache this for symmetric results
				MaximumInformationContentSimilarity mic = new MaximumInformationContentSimilarity();
				simEngine.calculateSimilarity(mic, pa, pb);
				mic.bestSubsumers = lcsxs; // treat all LCSs as best
				getBestMatch(bestMap, pa, mic);
				getBestMatch(bestMap, pb, mic);
			}
		}
		int n = 0;
		double sum = 0;
		for (OWLObject pa : aset) {
			sum += bestMap.get(pa).score;
			n++;
		}
		for (OWLObject pb : bset) {
			sum += bestMap.get(pb).score;
			n++;
		}
		setScore(sum/n);
		matchMap = bestMap;

	}

	void getBestMatch(Map <OWLObject,Similarity> bestMap, OWLObject x, Similarity r) {
		if (bestMap.containsKey(x)) {
			Similarity prev = bestMap.get(x);
			if (r.score > prev.score ) {
				bestMap.put(x, r);
			}
		}
		else {
			bestMap.put(x, r);
		}

	}
	
	// EXPERIMENTAL
	
	public void getSubgraph(OWLGraphWrapper g, OWLObject x, Set<OWLObject> subsumers) {
		Set<OWLObject> ancs = g.getAncestors(x);
		for (OWLObject a : ancs) {
			Set<OWLObject> ancs2 = g.getAncestorsReflexive(a);
			ancs2.retainAll(subsumers);
			if (ancs2.size() > 0) {
				
			}
		}
	}
	
	void getIntermediateNodes(OWLGraphWrapper g, OWLObject a, Set<OWLObject> subsumers) {
		for (OWLObject x : subsumers) {
			g.getEdgesBetween(a, x);
		}
	}
	
	public class LCS {
		OWLObject a;
		OWLObject b;
		OWLObject lcs;
	}

	/**
	 * Algorithm: traverse the two description trees in parallel, building a subsuming DT.
	 * if a branch is hit, branch the subsuming tree.
	 * at the end we prune branches that do not lead to a subsumer.
	 * 
	 * 
	 */
	public OWLClassExpression buildDescription(OWLGraphWrapper g, OWLObject a, OWLObject b, Set<OWLObject> subsumers) {
		// candidate expression
		OWLClassExpression x = null;
		
		if (subsumers.size() == 1) {
			// dual path up from here is linear, no intersections to be made.
			return (OWLClassExpression) subsumers.iterator().next();
		}
		
		// get best match for a in {b, desc-of-b}
		// also - restrict subsumers
		// TODO
		OWLObject bNext = b;
		
		Set<OWLGraphEdge> edges = g.getOutgoingEdges(a);
		for (OWLGraphEdge e : edges) {
			OWLObject t = e.getTarget();
			Set<OWLObject> ancs = g.getAncestorsReflexive(t);
			ancs.retainAll(subsumers);
			if (ancs.size() == 0) {
				continue;
			}
			else {
				Set<OWLClassExpression> subExprs = new HashSet<OWLClassExpression>();
				for (OWLObject anc : ancs) {
					subExprs.add(buildDescription(g,anc,bNext,subsumers));
				}
				if (subExprs.size() == 1) {
					x = subExprs.iterator().next();
				}
				else {
					x = g.getDataFactory().getOWLObjectIntersectionOf(subExprs);
				}
			}
		}
		return x;
		
	}
	
	private Set<OWLClassExpression> getNextMatches(Set<OWLObject> as,
			Set<OWLObject> bs) {
		// TODO Auto-generated method stub
		return null;
	}

	public Set<OWLObject> partitionSubsumers(OWLGraphWrapper g, OWLObject s, Set<OWLObject> subsumers) {
		Set<OWLObject> pset = new HashSet<OWLObject>();
		for (OWLObject t : subsumers) {
			if (g.getEdgesBetween(s, t).size() > 0) {
				pset.add(t);
			}
		}
		// todo - behavior if pset is identical
		return pset;		
	}


	public void print(PrintStream s) {
		s.println("Matches A");
		print(s, aset);
		s.println("Matches B");
		print(s, bset);
		s.println("Score: "+score);
	}

	private void print(PrintStream s, Set<OWLObject> oset) {
		for (OWLObject x : oset) {
			s.print(x);
			s.print(" ");
			Similarity sim = matchMap.get(x);
			s.print(sim.a+" -vs- "+sim.b+" "); // TODO -- one way
			s.println(sim.toString());
		}

	}
}
