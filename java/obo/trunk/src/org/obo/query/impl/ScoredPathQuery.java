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
import org.obo.query.Query;
import org.obo.query.StringQuery;
import org.obo.query.impl.TextQuery.HitType;

import org.apache.log4j.*;

public class ScoredPathQuery implements
	StringQuery<FieldPath, ScoredPathHit> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScoredPathQuery.class);

	protected List<FieldPathSpec> specs = new LinkedList<FieldPathSpec>();

	protected Map<SearchCriterion<IdentifiedObject, String>, Double> weightMap = new HashMap<SearchCriterion<IdentifiedObject, String>, Double>();

	protected boolean ignoreCase = true;

	protected Collection<String> searchWords;

	protected String searchString;

	public ScoredPathQuery() {
	}

	public void clear() {
		specs.clear();
		weightMap.clear();
	}
	
	public void addPathSpec(FieldPathSpec spec) {
		addPathSpec(spec, 1d);
	}

	public void addPathSpec(
			FieldPathSpec spec, Double weight) {
		specs.add(spec);
		weightMap.put(spec.getLastCriterion(), weight);
	}

	public double getWeight(SearchCriterion<IdentifiedObject, String> criterion) {
		Double d = weightMap.get(criterion);
		if (d == null)
			return 0;
		else
			return d;
	}

	public FieldPath convertToInputType(ScoredPathHit original) {
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
	

	public ScoredPathHit matches(FieldPath a) {
		ScoredPathHit scoredQueryHit = null;
		LinkedList<ScoredStringHit> hits = new LinkedList<ScoredStringHit>();
		LinkedList<String> scratch = new LinkedList<String>();
		double score = 0;
		for (FieldPathSpec spec : specs) {
			SearchCriterion criterion = spec.getLastCriterion();
			scratch.clear();
			criterion.getValues(scratch, a.getLastValue());
			ScoredCriterionHit scoredCriterionHit = null;
			boolean failed = false;
			for (String s : scratch) {
				s = getString(s);
				for (String searchWord : searchWords) {
					int pos = s.indexOf(getString(searchWord));
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
					scoredQueryHit = new ScoredPathHit(path);
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

	public Comparator<ScoredPathHit> getComparator() {
		return ScoredPathHit.COMPARATOR;
	}

	public Collection<ScoredPathHit> createResultHolder() {
		return new ArrayList<ScoredPathHit>();
	}

	public Class<FieldPath> getInputType() {
		return FieldPath.class;
	}

	public ScoredPathHit convertToOutputType(FieldPath original) {
		/*
		ScoredStringHit strHit = new ScoredStringHit(original.getName(),
				original.getName(), Double.MAX_VALUE, 0);
		ScoredCriterionHit scHit = new ScoredCriterionHit(
				new NameSearchCriterion(), 1, original.getName());
		scHit.addStringHit(strHit);
		ScoredPathHit hit = new ScoredPathHit(new FieldPath(original));
		hit.addCriterionHit(scHit);
		return hit;
		*/
		return null;
	}

	protected Comparator<SearchCriterion<IdentifiedObject, String>> weightComparator = new Comparator<SearchCriterion<IdentifiedObject, String>>() {
		public int compare(SearchCriterion<IdentifiedObject, String> o1,
				SearchCriterion<IdentifiedObject, String> o2) {
			return (int) (getWeight(o2) - getWeight(o1));
		}
	};

	protected FieldPath path;

	public Comparator<SearchCriterion<IdentifiedObject, String>> getWeightComparator() {
		return weightComparator;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return specs;
	}

	public void setFieldPath(FieldPath path) {
		this.path = path;		
	}
}

