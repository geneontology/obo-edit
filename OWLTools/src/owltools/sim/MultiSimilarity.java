package owltools.sim;

import java.io.PrintStream;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;
import owltools.sim.SimEngine.SimilarityAlgorithmException;

public class MultiSimilarity extends Similarity {

	public OWLClass comparisonClass = null;
	public OWLObjectProperty comparisonProperty = null;
	public String subSimMethod = "DescriptionTreeSimilarity";

	public Map<OWLObject,Set<OWLObject>> featureToAttributeMap;
	Map<OWLObject,Similarity> aBest;
	Map<OWLObject,Similarity> bBest;

	class ValueComparator implements Comparator {

		Map base;
		public ValueComparator(Map base) {
			this.base = base;
		}

		public int compare(Object a, Object b) {
			if(((Similarity)base.get(a)).score < ((Similarity)base.get(b)).score) {
				return 1;
			} else if(((Similarity)base.get(a)).score == ((Similarity)base.get(b)).score) {
				return 0;
			} else {
				return -1;
			}
		}
	}

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b)
	throws SimilarityAlgorithmException {
		this.simEngine = simEngine;
		createFeatureToAttributeMap();
		calculate(a,b,featureToAttributeMap.get(a),featureToAttributeMap.get(b));
	}

	private void calculate(OWLObject a, OWLObject b, Set<OWLObject> aAtts,
			Set<OWLObject> bAtts) throws SimilarityAlgorithmException {
		
		if (aAtts.size() == 0 || bAtts.size() ==0) {
			isComparable = false;
			return;
		}
		Map<OWLObjectPair,Double> smap = new HashMap<OWLObjectPair,Double>();

		aBest = new HashMap<OWLObject,Similarity>();
		bBest = new HashMap<OWLObject,Similarity>();
		for (OWLObject aAtt : aAtts) {
			for (OWLObject bAtt : bAtts) {
				Similarity ss = simEngine.getSimilarityAlgorithm(subSimMethod);
				ss.calculate(simEngine, aAtt, bAtt);
				Double sc = ss.getScore();
				smap.put(new OWLObjectPair(aAtt,bAtt), sc);
				if (!aBest.containsKey(aAtt) ||
						sc > aBest.get(aAtt).getScore()) {
					aBest.put(aAtt, ss);
				}
				if (!bBest.containsKey(bAtt) ||
						sc > bBest.get(bAtt).getScore()) {
					bBest.put(bAtt, ss);
				}

			}
		}
		double totalSc = 0.0;
		int n = 0;
		Set <OWLObject>allAtts = new HashSet<OWLObject>();
		allAtts.addAll(aAtts);
		allAtts.addAll(bAtts);


		for (OWLObject att : aAtts) {
			totalSc += aBest.get(att).getScore();
			n++;
		}
		for (OWLObject att : bAtts) {
			totalSc += bBest.get(att).getScore();
			n++;
		}
		this.score = totalSc / n;
	}

	public void createFeatureToAttributeMap() {
		featureToAttributeMap = new HashMap<OWLObject,Set<OWLObject>>();
		featureToAttributeMap.put(a, getAttributesFor(a));
		featureToAttributeMap.put(b, getAttributesFor(b));
	}

	public Set<OWLObject> getAttributesFor(OWLObject x) {
		OWLGraphWrapper g = simEngine.getGraph();
		Set<OWLObject> ancs = new HashSet<OWLObject>();
		if (comparisonClass != null) {
			for (OWLGraphEdge e : g.getOutgoingEdgesClosure(x)) {
				OWLObject t = e.getTarget();
				if (g.getSubsumersFromClosure(t).contains(comparisonClass)) {
					ancs.add(t);
				}
			}
		}
		if (comparisonProperty != null) {
			for (OWLGraphEdge e : g.getOutgoingEdgesClosure(x)) {
				OWLObject t = e.getTarget();
				List<OWLQuantifiedProperty> qpl = e.getQuantifiedPropertyList();
				if (qpl.size() == 1) {
					if (comparisonProperty.equals(qpl.get(0).getProperty())) {
						ancs.add(t);
					}
				}
			}

		}
		simEngine.makeNonRedundant(ancs);
		return ancs;
	}

	public Set<OWLObject> sortMapByScore(Map<OWLObject,Similarity> map) {
		ValueComparator bvc =  new ValueComparator(map);
		TreeMap<OWLObject,Similarity> sorted_map = new TreeMap(bvc);
		sorted_map.putAll(map);
		return sorted_map.keySet();
	}

	public void print(PrintStream s) {
		s.print("COMPARISON:");
		print(s,a);
		s.print(" vs ");
		print(s,b);
		s.println();
		if (!isComparable) {
			s.println("**not comparable");
			return;
		}
		s.println("AVG-BEST: "+score);
		s.println("BEST-MATCHES(A): "+aBest.keySet().size());
		for (OWLObject att : sortMapByScore(aBest)) {
			printX(s,att,aBest,bBest,aBest.get(att).b);
		}
		s.println("BEST-MATCHES(B): "+bBest.keySet().size());
		for (OWLObject att : sortMapByScore(bBest)) {
			printX(s,att,bBest,aBest,bBest.get(att).a);
		}
	}

	private void printX(PrintStream s, OWLObject att, Map<OWLObject, Similarity> bestMap, Map<OWLObject, Similarity> bestMap2, OWLObject bestMapObj) {
		Similarity bestmatch = bestMap.get(att);
		s.println("  Score: "+bestmatch.getScore());
		//if (bestMap2.get(att).score == bestmatch.getScore())
		//	s.println("  **reciprocal**");
		s.print("  ");
		printDescription(s,att);
		s.print(" -vs- ");
		printDescription(s,bestMapObj);
		s.println();
		s.print("  A:");
		s.println(att);
		s.print("  B:");
		s.print(bestMapObj);
		s.println();
		if (bestmatch instanceof DescriptionTreeSimilarity) {
			s.print("  Shared:");
			printDescription(s, ((DescriptionTreeSimilarity)bestmatch).lcs);
			s.println();
		}
		s.println();

	}

	/**
	 * adds additional axioms specific to this method.
	 * Creates a named LCS class equivalent to the generated expression
	 * 
	 * @param id
	 * @param result
	 * @param axioms
	 */
	@Override
	protected void translateResultsToOWLAxioms(String id, OWLNamedIndividual result, Set<OWLAxiom> axioms) {
		OWLDataFactory df = simEngine.getGraph().getDataFactory();
		
		OWLAnnotationProperty pa = df.getOWLAnnotationProperty(annotationIRI("has_best_match_for_object_1"));
		OWLAnnotationProperty pb = df.getOWLAnnotationProperty(annotationIRI("has_best_match_for_object_2"));

		for (OWLObject att : aBest.keySet()) {
			axioms.addAll(aBest.get(att).translateResultsToOWLAxioms());
			axioms.add(df.getOWLAnnotationAssertionAxiom(pa, result.getIRI(), aBest.get(att).persistentIRI));
		}
		for (OWLObject att : bBest.keySet()) {
			axioms.addAll(bBest.get(att).translateResultsToOWLAxioms());
			axioms.add(df.getOWLAnnotationAssertionAxiom(pb, result.getIRI(), bBest.get(att).persistentIRI));
		}
		/*
		s.println("BEST-MATCHES(B): "+bBest.keySet().size());
		for (OWLObject att : sortMapByScore(bBest)) {
			printX(s,att,bBest,aBest,bBest.get(att).a);
		}

		// declare a named class for the LCS and make this equivalent to the anonymous expression
		OWLClass namedLCS = df.getOWLClass(IRI.create(id+"_LCS"));
		axioms.add(df.getOWLAnnotationAssertionAxiom(df.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI()),
				namedLCS.getIRI(), 
				df.getOWLTypedLiteral("LCS of "+simEngine.label(a)+" and "+simEngine.label(b))));
		axioms.add(df.getOWLEquivalentClassesAxiom(namedLCS, lcs));

		// link the similarity object to the named LCS
		OWLAnnotationProperty lcsp = df.getOWLAnnotationProperty(annotationIRI("has_least_common_subsumer"));
		axioms.add(df.getOWLAnnotationAssertionAxiom(lcsp, result.getIRI(), namedLCS.getIRI()));
		*/
	}


}
