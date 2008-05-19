package org.obo.query.impl;

import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.Map;

import org.obo.datamodel.FieldPath;
import org.obo.filters.SearchCriterion;

import org.apache.log4j.*;

public class ScoredPathHit extends BasicSearchHit<FieldPath> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScoredPathHit.class);

	protected FieldPath object;

	protected Collection<ScoredCriterionHit> criterionHits;

	public static final Comparator<ScoredPathHit> COMPARATOR = new Comparator<ScoredPathHit>() {
		public int compare(ScoredPathHit o1, ScoredPathHit o2) {
			int val = (int) (o2.getScore() - o1.getScore());
			if (val == 0) {
				return o1.getString().compareToIgnoreCase(
						o2.getString());
			} else
				return val;
		}
	};

	public ScoredPathHit(FieldPath object) {
		super(object);
	}
	
	public String getString() {
		return object.getLastValue().toString();
	}
	
	public Collection<ScoredCriterionHit> getHitsForCriterion(
			SearchCriterion crit) {
		return getHitsForCriterion(crit.getID());
	}

	public Collection<ScoredCriterionHit> getHitsForCriterion(
			String critID) {
		Collection<ScoredCriterionHit> out = new LinkedList<ScoredCriterionHit>();
		for(ScoredCriterionHit hit : criterionHits) {
			if (hit.getCriterion().getID().equals(critID))
				out.add(hit);
		}
		return out;
	}

	public void addCriterionHit(ScoredCriterionHit hit) {
		if (criterionHits == null)
			criterionHits = new LinkedList<ScoredCriterionHit>();
		criterionHits.add(hit);
	}

	public double getScore() {
		double score = 0;
		for (ScoredCriterionHit hit : criterionHits)
			score += hit.getScore();
		return score;
	}

	public Collection<ScoredCriterionHit> getCriterionHits() {
		return criterionHits;
	}
}
