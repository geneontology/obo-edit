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
	Double lcsIC;
	public Set<OWLObject> bestSubsumers = new HashSet<OWLObject>();
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (OWLObject obj : bestSubsumers) {
			sb.append(obj+"; ");
		}
		return score + " "+sb.toString();
	}

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) {
		this.simEngine = simEngine;
		Set<OWLObject> objs = simEngine.getLeastCommonSubsumers(a, b);
		LOG.info("LCSs:"+objs.size());
		lcsIC = simEngine.getInformationContent(objs);
		if (lcsIC == null) {
			lcsIC = 0.0;
			setScore(lcsIC);
		}
		else {
			Double aIC = simEngine.getInformationContent(a);
			Double bIC = simEngine.getInformationContent(b);
			if (aIC == null || bIC == null) {
				setScore(0.0);
			}
			setScore( lcsIC / Math.min(aIC, bIC));
		}
		
		this.bestSubsumers = objs;

	}

	@Override
	protected void translateResultsToOWLAxioms(String id,
			OWLNamedIndividual result, Set<OWLAxiom> axioms) {
		// TODO Auto-generated method stub
		
	}
	
	public void print(PrintStream s) {
		s.println("IntersectionIC:"+toString()+"\n");
		for (OWLObject obj : bestSubsumers) {
			print(s,obj);
		}

	}

}
