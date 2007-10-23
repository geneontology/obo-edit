package org.obo.query.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Synonym;
import org.obo.filters.NameSearchCriterion;
import org.obo.filters.SearchCriterion;
import org.obo.filters.StringConverter;
import org.obo.filters.StringCriterionWrapper;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.query.Query;
import org.obo.query.StringQuery;
import org.obo.query.impl.TextQuery.HitType;

public class ScoredCriterionQuery implements
		StringQuery<IdentifiedObject, ScoredQueryHit> {

	protected List<SearchCriterion<IdentifiedObject, String>> searchCriteria = new LinkedList<SearchCriterion<IdentifiedObject, String>>();

	protected Map<SearchCriterion<IdentifiedObject, String>, Double> weightMap = new HashMap<SearchCriterion<IdentifiedObject, String>, Double>();

	protected boolean ignoreCase = true;

	protected Collection<String> searchWords;

	protected String searchString;

	public ScoredCriterionQuery(boolean useDefaults) {
		if (useDefaults)
			populateDefaults();
	}

	protected void populateDefaults() {
		addCriterion(new NameSearchCriterion(), 100d);
		addCriterion(new StringCriterionWrapper<IdentifiedObject, Synonym>(
				new SynonymSearchCriterion(), StringConverter.DEFAULT), 1d);
	}

	public void clear() {
		searchCriteria.clear();
		weightMap.clear();
	}

	public void addCriterion(SearchCriterion<IdentifiedObject, String> criterion) {
		addCriterion(criterion, 1d);
	}

	public void addCriterion(
			SearchCriterion<IdentifiedObject, String> criterion, Double weight) {
		searchCriteria.add(criterion);
		weightMap.put(criterion, weight);
	}

	public double getWeight(SearchCriterion<IdentifiedObject, String> criterion) {
		Double d = weightMap.get(criterion);
		if (d == null)
			return 0;
		else
			return d;
	}

	public IdentifiedObject convertToInputType(ScoredQueryHit original) {
		return original.getHit();
	}

	public void setIgnoreCase(boolean ignoreCase) {
		this.ignoreCase = ignoreCase;
	}

	public void setSearchString(String searchString) {
		searchWords = new LinkedList<String>();
		StringTokenizer tokenizer = new StringTokenizer(searchString);
		while (tokenizer.hasMoreTokens())
			searchWords.add(tokenizer.nextToken());
		this.searchString = searchString;
	}

	public String getString(String s) {
		if (ignoreCase)
			return s.toLowerCase();
		else
			return s;
	}

	public ScoredQueryHit matches(IdentifiedObject a) {
		ScoredQueryHit scoredQueryHit = null;
		LinkedList<ScoredStringHit> hits = new LinkedList<ScoredStringHit>();
		LinkedList<String> scratch = new LinkedList<String>();
		double score = 0;
		for (SearchCriterion<IdentifiedObject, String> criterion : searchCriteria) {
			scratch.clear();
			criterion.getValues(scratch, a);
			ScoredCriterionHit scoredCriterionHit = null;
			boolean failed = false;
			for (String s : scratch) {
				s = getString(s);
				for (String searchWord : searchWords) {
					int pos = getString(s).indexOf(getString(searchWord));
					double wordscore = getMatchPosScore(pos, s);
					if (wordscore > 0) {
						ScoredStringHit scoredStringHit = new ScoredStringHit(
								searchWord, s, wordscore, pos);
						if (scoredCriterionHit == null) {
							scoredCriterionHit = new ScoredCriterionHit(
									criterion, weightMap.get(criterion), s);
						}
						scoredCriterionHit.addStringHit(scoredStringHit);
					} else {
						failed = true;
						break;
					}
				}
				if (failed) {
					scoredCriterionHit = null;
					break;
				}
			}
			if (scoredCriterionHit != null) {
				if (scoredQueryHit == null) {
					scoredQueryHit = new ScoredQueryHit(a);
				}
				scoredQueryHit.addCriterionHit(scoredCriterionHit);
			}
		}

		return scoredQueryHit;
	}

	protected static int getMatchPosScore(int index, String str) {
		if (index >= 0) {
			if (index == 0)
				return 10;
			else if (Character.isWhitespace(str.charAt(index - 1)))
				return 5;
			else
				return 1;
		} else
			return 0;
	}

	public Collection<SearchCriterion<IdentifiedObject, String>> getCriteria() {
		return searchCriteria;
	}

	public Comparator<ScoredQueryHit> getComparator() {
		return ScoredQueryHit.COMPARATOR;
	}

	public Collection<ScoredQueryHit> createResultHolder() {
		return new ArrayList<ScoredQueryHit>();
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	public ScoredQueryHit convertToOutputType(IdentifiedObject original) {
		ScoredStringHit strHit = new ScoredStringHit(original.getName(),
				original.getName(), Double.MAX_VALUE, 0);
		ScoredCriterionHit scHit = new ScoredCriterionHit(
				new NameSearchCriterion(), 1, original.getName());
		scHit.addStringHit(strHit);
		ScoredQueryHit hit = new ScoredQueryHit(original);
		hit.addCriterionHit(scHit);
		return hit;
	}

	protected Comparator<SearchCriterion<IdentifiedObject, String>> weightComparator = new Comparator<SearchCriterion<IdentifiedObject, String>>() {
		public int compare(SearchCriterion<IdentifiedObject, String> o1,
				SearchCriterion<IdentifiedObject, String> o2) {
			return (int) (getWeight(o2) - getWeight(o1));
		}
	};

	public Comparator<SearchCriterion<IdentifiedObject, String>> getWeightComparator() {
		return weightComparator;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		LinkedList<FieldPathSpec> specs = new LinkedList<FieldPathSpec>();
		for(SearchCriterion<IdentifiedObject, String> sc : searchCriteria) {
			FieldPathSpec spec = new FieldPathSpec(sc);
			specs.add(spec);
		}
		return specs;
	}

	public void setFieldPath(FieldPath path) {
	}
}
