package org.obo.query.impl;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.SearchCriterion;

public class ScoredCriterionHit {

	protected SearchCriterion<IdentifiedObject, String> criterion;

	protected List<ScoredStringHit> hits = new LinkedList<ScoredStringHit>();

	double weight = 0;

	protected String searchString;

	public static final Comparator<ScoredCriterionHit> WEIGHT_COMPARATOR = new Comparator<ScoredCriterionHit>() {
		public int compare(ScoredCriterionHit o1, ScoredCriterionHit o2) {
			return (int) (o2.getWeight() - o1.getWeight());
		}
	};

	public ScoredCriterionHit(
			SearchCriterion<IdentifiedObject, String> criterion, double weight,
			String searchString) {
		this.criterion = criterion;
		this.weight = weight;
		this.searchString = searchString;
	}
	
	public String getSearchString() {
		return searchString;
	}

	public Map<String, Collection<ScoredStringHit>> getHitMap() {
		Map<String, Collection<ScoredStringHit>> out = new HashMap<String, Collection<ScoredStringHit>>();
		for (ScoredStringHit hit : hits) {
			Collection<ScoredStringHit> hitCollection = out.get(hit
					.getMatchString());
			if (hitCollection == null) {
				hitCollection = new LinkedList<ScoredStringHit>();
				out.put(hit.getMatchString(), hitCollection);
			}
			hitCollection.add(hit);
		}
		return out;
	}

	public void addStringHit(ScoredStringHit scoredStringHit) {
		hits.add(scoredStringHit);
	}

	public Collection<ScoredStringHit> getHits() {
		return hits;
	}

	public double getScore() {
		double score = 0;
		double inOrderMultiplier = 1;
		ScoredStringHit lastHit = null;
		for (ScoredStringHit hit : hits) {
			if (lastHit != null) {
				if (lastHit.getHitPos() > hit.getHitPos())
					inOrderMultiplier = 1;
				else
					inOrderMultiplier++;
			}
			score += hit.getScore();
			lastHit = hit;
		}
		return weight * score * inOrderMultiplier;
	}

	public SearchCriterion<IdentifiedObject, String> getCriterion() {
		return criterion;
	}

	public double getWeight() {
		return weight;
	}
}
