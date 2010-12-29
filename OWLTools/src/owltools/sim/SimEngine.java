package owltools.sim;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.sim.Similarity;

public class SimEngine {

	// -------------------------------------
	// Similarity Algorithms
	// -------------------------------------

	/*
	public abstract class SimilarityAlgorithm {};
	public class JaccardSimilarity extends SimilarityAlgorithm {};
	public class OverlapSimilarity extends SimilarityAlgorithm {};
	public class MaximumInformationContentSimilarity extends SimilarityAlgorithm {};
	public class SumInformationContentBestMatchesSimilarity extends SimilarityAlgorithm {};
	 */

	// -------------------------------------
	// Similarity Result Classes
	// -------------------------------------





	// -------------------------------------
	// Class Attributes
	// -------------------------------------

	protected OWLGraphWrapper graph;

	// -------------------------------------
	// Constructions
	// -------------------------------------

	public SimEngine(OWLGraphWrapper wrapper) {
		setGraph(wrapper);
	}

	// -------------------------------------
	// getters/setters
	// -------------------------------------

	public OWLGraphWrapper getGraph() {
		return graph;
	}

	public void setGraph(OWLGraphWrapper graph) {
		this.graph = graph;
	}

	// -------------------------------------
	// Utils
	// -------------------------------------

	public Similarity getSimilarityAlgorithm(String name) throws SimilarityAlgorithmException {
		Class c = null;
		try {
			c = Class.forName(name);
		} catch (ClassNotFoundException e) {
			if (name.contains(".")) {
				e.printStackTrace();
				throw new SimilarityAlgorithmException(name);
			}
			//return getSimilarityAlgorithm("owltools.sim.SimEngine$"+name);
			return getSimilarityAlgorithm("owltools.sim."+name);
		}
		return getSimilarityAlgorithm(c);
	}

	public Similarity getSimilarityAlgorithm(Class c) throws SimilarityAlgorithmException {
		try {
			// http://stackoverflow.com/questions/728842/instantiating-an-inner-class
			//Constructor<Similarity> ctor = c.getConstructor(SimEngine.class);
			//return (Similarity) ctor.newInstance(this);
			return (Similarity) c.newInstance();
		} catch (Exception e) {
			e.printStackTrace();
			throw new SimilarityAlgorithmException("cannot make new instance of "+c);
		}
	}

	public Class[] getAllSimilarityAlgorithmClasses() {
		Class[] ms = {
				JaccardSimilarity.class,
				OverlapSimilarity.class,
				DescriptionTreeSimilarity.class,
				MaximumInformationContentSimilarity.class,
				AvgInformationContentBestMatchesSimilarity.class
		};
		return ms;
	}


	// -------------------------------------
	// Statistics
	// -------------------------------------


	public int getCorpusSize() {
		// TODO - add filtering
		return graph.getAllOWLObjects().size();
	}

	public int getFrequency(OWLObject obj) {
		return graph.getDescendants(obj).size();	
	}
	public Double getInformationContent(OWLObject obj) {
		return -Math.log(((double) (getFrequency(obj)) / getCorpusSize())) / Math.log(2);
	}
	public boolean hasInformationContent(OWLObject obj) {
		return getFrequency(obj) > 0;
	}


	public Set<OWLObject> getUnionSubsumers(OWLObject a, OWLObject b) {
		Set<OWLObject> s1 = getGraph().getAncestorsReflexive(a);
		s1.addAll(getGraph().getAncestorsReflexive(b));
		return s1;
	}
	public int getUnionSubsumersSize(OWLObject a, OWLObject b) {
		return getUnionSubsumers(a,b).size();		
	}

	public Set<OWLObject> getCommonSubsumers(OWLObject a, OWLObject b) {
		Set<OWLObject> s1 = getGraph().getNamedAncestorsReflexive(a);
		s1.retainAll(getGraph().getNamedAncestorsReflexive(b));
		return s1;
	}
	public int getCommonSubsumersSize(OWLObject a, OWLObject b) {
		return getCommonSubsumers(a,b).size();		
	}

	public Set<OWLObject> getLeastCommonSubsumers(OWLObject a, OWLObject b) {
		Set<OWLObject> objs = getCommonSubsumers(a,b);	
		return makeNonRedundant(objs);
	}
	public int getLeastCommonSubsumersSize(OWLObject a, OWLObject b) {
		return getLeastCommonSubsumers(a,b).size();		
	}

	public Set<OWLObject> makeNonRedundant(Set<OWLObject> objs) {
		Set<OWLObject> rs = new HashSet<OWLObject>();
		for (OWLObject obj : objs) {
			Set<OWLObject> ancs = getAncestors(obj);
			ancs.retainAll(objs);
			ancs.remove(obj);
			// todo - equivalence
			for (OWLObject anc : ancs) {
				if (!getAncestors(anc).contains(obj))
					rs.add(anc);
			}
		}
		objs.removeAll(rs);
		return objs;
	}

	private Set<OWLObject> getAncestors(OWLObject obj) {
		// TODO configurable
		return getGraph().getAncestorsReflexive(obj);
	}


	public Double calculateSimilarityScore(Similarity m, OWLObject a, OWLObject b) throws SimilarityAlgorithmException {
		Similarity r = calculateSimilarity(m,a,b);
		return r.score;
	}

	/**
	 * Calculates the similarity between two OWLObjects
	 * 
	 * this is the core method of the SimEngine
	 * 
	 * @param m
	 * @param a
	 * @param b
	 * @return
	 * @throws SimilarityAlgorithmException
	 */
	public Similarity calculateSimilarity(Similarity m, OWLObject a, OWLObject b) throws SimilarityAlgorithmException {
		// TODO - consider making these immutable and forcing a constructor
		m.a = a;
		m.b = b;
		m.calculate(this,a,b);
		return m;
	}

	// consider moving to class that uses it..
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

	public Set<Similarity> calculateAllSimilarity(OWLObject a, OWLObject b) throws SimilarityAlgorithmException  {
		Set<Similarity> mm = 
			new HashSet<Similarity>();
		for (Class mc : getAllSimilarityAlgorithmClasses()) {
			Similarity m = null;
			try {
				m = getSimilarityAlgorithm(mc);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (m != null) {
				calculateSimilarity(m,a,b);
				mm.add(m);
			}
		}
		return mm;
	}


	// TODO
	public void calculateSimilarityAllByAll(Similarity m) throws SimilarityAlgorithmException {
		for (OWLObject a : graph.getAllOWLObjects()) {
			if (excludeObjectFromComparison(a))
				continue;
			for (OWLObject b : graph.getAllOWLObjects()) {
				if (a.equals(b))
					continue;
				if (excludeObjectFromComparison(b))
					continue;
				calculateSimilarity(m,a,b);
				Double s = m.score;
				System.out.println(a+" "+b+" = "+s);
				m.print();
			}
		}
	}

	private boolean excludeObjectFromComparison(OWLObject a) {
		// TODO configurable
		return !(a instanceof OWLNamedIndividual);
	}

	public OWLObject createUnionExpression(OWLObject a, OWLObject b, OWLObject c) {
		Set<OWLGraphEdge> edgesA = graph.getEdgesBetween(a, c);
		Set<OWLGraphEdge> edgesB = graph.getEdgesBetween(b, c);
		if (edgesA.equals(edgesB)) {
			return edgeSetToExpression(edgesA);
		}
		else {
			OWLClassExpression xa = edgeSetToExpression(edgesA);
			OWLClassExpression xb = edgeSetToExpression(edgesA);
			HashSet<OWLClassExpression> xl = new HashSet<OWLClassExpression>();
			xl.add(xa);
			xl.add(xb);
			if (xl.size() == 1)
				return xl.iterator().next();
			OWLObjectUnionOf xu = graph.getDataFactory().getOWLObjectUnionOf(xl);
			return xu;
		}
	}
	
	public OWLClassExpression edgeSetToExpression(Set<OWLGraphEdge> edges) {
		Set<OWLClassExpression> xs = new HashSet<OWLClassExpression>();
		for (OWLGraphEdge e : edges) {
			OWLObject x = graph.edgeToTargetExpression(e);
			xs.add((OWLClassExpression)x);
		}
		if (xs.size() == 1)
			return xs.iterator().next();
		OWLObjectIntersectionOf ix = graph.getDataFactory().getOWLObjectIntersectionOf(xs);
		return ix;
	}

	// -------------------------------------
	// Exception Classes
	// -------------------------------------

	public class SimilarityAlgorithmException extends Exception {

		public SimilarityAlgorithmException(String m) {
			super(m);
		}

	}

}
