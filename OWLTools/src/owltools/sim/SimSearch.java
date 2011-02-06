package owltools.sim;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.log4j.Logger;
import org.semanticweb.owlapi.model.OWLObject;

import owltools.util.BloomFilter;

/**
 * quick search for best matches
 * 
 * @author cjm
 *
 */
public class SimSearch {
	
	private static Logger LOG = Logger.getLogger(SimSearch.class);

	protected Double minScore;
	protected Double minIC = 3.5; // note that with test datasets this is too strict
	protected SimEngine simEngine;
	protected BloomFilter<OWLObject> bloomFilter;
	protected int maxHits = 1000;
	protected Set<OWLObject> candidates;
	

	public SimSearch(SimEngine simEngine) {
		super();
		this.simEngine = simEngine;
	}

	public List<OWLObject> search(OWLObject queryObj) {
		List<OWLObject> hits = new ArrayList<OWLObject>(maxHits);
		System.out.println("gettings atts for "+queryObj+" -- "+simEngine.comparisonProperty);
		Set<OWLObject> atts = simEngine.getAttributeClosureFor(queryObj);
		System.out.println("all atts: "+atts.size());
		if (atts.size() == 0)
			return hits;
		
		// only compare using significant atts;
		// we don't do the same test on candidates as these will be removed by the
		// intersection operation. they will have a small effect on the score, as
		// we don't divide by the union, but instead the sum of sizes
		atts = filterNonSignificantAttributes(atts);

		//bloomFilter = new BloomFilter<OWLObject>(0.05, atts.size());
		//bloomFilter.addAll(atts);
				
		SortedMap<Integer,Set<OWLObject>> scoreCandidateMap = new TreeMap<Integer,Set<OWLObject>>();
		
		for (OWLObject candidate : getCandidates()) {
			if (candidate.equals(queryObj))
				continue;
			Set<OWLObject> iAtts = simEngine.getAttributeClosureFor(candidate);
			System.out.println("catts:"+iAtts.size());
			//Set<OWLObject> iAtts = simEngine.getGraph().getAncestors(candidate);

			if (iAtts.size() == 0)
				continue;
			int cAttsSize = iAtts.size();
	
			iAtts.retainAll(atts);
			//Collection<OWLObject> iAtts = bloomFilter.intersection(cAtts);
			
			// simJ, one-sided, scaled by 1000
			// negate to ensure largest first
			Integer score = - (iAtts.size() * 1000 / cAttsSize);
			if (!scoreCandidateMap.containsKey(score)) 
				scoreCandidateMap.put(score, new HashSet<OWLObject>());
			scoreCandidateMap.get(score).add(candidate);
			LOG.debug(score + " CANDIDATE:"+candidate);
			System.out.println("SIMJ2:\t"+score + "\t"+candidate+"\t"+iAtts.size()+"\t"+cAttsSize);
		}
		
		int n = 0;
		for (Set<OWLObject> cs : scoreCandidateMap.values()) {
			n += cs.size();
			hits.addAll(cs);
			if (n >= maxHits)
				break;
		}
		
		return hits;
	}
	

	private Set<OWLObject> filterNonSignificantAttributes(Set<OWLObject> atts) {
		Set<OWLObject> fAtts = new HashSet<OWLObject>();
		for (OWLObject att : atts) {
			if (simEngine.isExcludedFromAnalysis(att))
				continue;
			Double ic = simEngine.getInformationContent(att);
			if (ic != null && ic > minIC )
				fAtts.add(att);
		}
		return fAtts;
	}

	private Set<OWLObject> getCandidates() {
		if (candidates == null) {
			candidates = new HashSet<OWLObject>();
			candidates.addAll(simEngine.getGraph().getSourceOntology().getIndividualsInSignature());
		}
		return candidates;
	}
	
	public void setCandidates(Set<OWLObject> candidates) {
		this.candidates = candidates;
	}


	public SimEngine getSimEngine() {
		return simEngine;
	}

	public void setSimEngine(SimEngine simEngine) {
		this.simEngine = simEngine;
	}

	public int getMaxHits() {
		return maxHits;
	}

	public void setMaxHits(int maxHits) {
		this.maxHits = maxHits;
	}

	
	

}
