package owltools.sim;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;

import org.apache.log4j.Logger;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

/**
 * score is the IC of the intersection of all attributes  divided my min IC of a or b
 * 
 * it's recommended this used as a sub-method of a MultiSimilarity check
 * 
 * @author cjm
 *
 */
public class ConjunctiveSetInformationContentRatioSimilarity extends Similarity {

	private static Logger LOG = Logger.getLogger(ConjunctiveSetInformationContentRatioSimilarity.class);
	Double lcsICRatio;
	public Set<OWLObject> lcsIntersectionSet = new HashSet<OWLObject>();
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (OWLObject obj : lcsIntersectionSet) {
			sb.append(obj+"; ");
		}
		return score + " "+sb.toString();
	}

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) {
		this.simEngine = simEngine;
		this.a = a;
		this.b = b;
		Set<OWLObject> objs = simEngine.getLeastCommonSubsumers(a, b);
		LOG.info("LCSs:"+objs.size());
		score = simEngine.getInformationContent(objs);
		if (score == null) {
			score = 0.0;
			lcsICRatio = score;
		}
		else {
			Double aIC = simEngine.getInformationContent(a);
			Double bIC = simEngine.getInformationContent(b);
			if (aIC == null || bIC == null) {
				lcsICRatio = 0.0;
			}
			lcsICRatio = score / Math.min(aIC, bIC);
		}
		
		this.lcsIntersectionSet = objs;

	}

	@Override
	protected void translateResultsToOWLAxioms(String id,
			OWLNamedIndividual result, Set<OWLAxiom> axioms) {
		// TODO Auto-generated method stub
		
	}
	
	// -------------
	// REPORTING
	// -------------
	public void report(Reporter r) {
		r.report(this,"pair_match_ic_icratio_subsumer",a,b,score,lcsICRatio,lcsIntersectionSet);
	}

	// -------------
	// DISPLAY
	// -------------
	public void print(PrintStream s) {
		s.println("IntersectionIC:"+toString()+"\n");
		for (OWLObject obj : lcsIntersectionSet) {
			print(s,obj);
		}

	}

}
