package org.obo.query.impl;

import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.Map;

import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.SearchCriterion;

public class ScoredQueryHit extends BasicSearchHit<IdentifiedObject> {

	protected IdentifiedObject object;

	protected Collection<ScoredCriterionHit> criterionHits;

	public static final Comparator<ScoredQueryHit> COMPARATOR = new Comparator<ScoredQueryHit>() {
		public int compare(ScoredQueryHit o1, ScoredQueryHit o2) {
			int val = (int) (o2.getScore() - o1.getScore());
			if (val == 0) {
				return o1.getHit().getName().compareToIgnoreCase(
						o2.getHit().getName());
			} else
				return val;
		}
	};

	public ScoredQueryHit(IdentifiedObject object) {
		super(object);
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
