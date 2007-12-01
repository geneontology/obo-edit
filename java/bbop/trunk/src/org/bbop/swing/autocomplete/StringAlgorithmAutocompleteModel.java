package org.bbop.swing.autocomplete;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.StringUtil;
import org.bbop.util.TaskDelegate;

public class StringAlgorithmAutocompleteModel implements
		AutocompleteModel<StringAlgorithmAutocompleteModel.StringPair, String> {

	public static class StringPair {
		protected String original;
		protected String generated;

		public StringPair(String original, String generated) {
			this.original = original;
			this.generated = generated;
		}

		public String getOriginal() {
			return original;
		}

		public String getGenerated() {
			return generated;
		}
	}

	public static interface StringGenerator {
		public Collection<String> generateStrings(String input);
	}

	protected StringGenerator algorithm;
	
	public StringAlgorithmAutocompleteModel() {
		this(null);
	}
	
	public StringAlgorithmAutocompleteModel(StringGenerator algorithm) {
		setAlgorithm(algorithm);
	}
	
	public void setAlgorithm(StringGenerator algorithm) {
		this.algorithm = algorithm;
	}

	public StringPair createValue(String val) {
		throw new UnsupportedOperationException();
	}

	public Collection<StringPair> getAllValues() {
		return Collections.emptySet();
	}

	public Class<StringPair> getDisplayType() {
		return StringPair.class;
	}

	public List<StringPair> getDisplayValues(String val) {
		Collection<String> strs = algorithm.generateStrings(val);
		List<StringPair> out = new LinkedList<StringPair>();
		out.add(new StringPair(val, val));
		for (String str : strs) {
			out.add(new StringPair(val, str));
		}
		return out;
	}

	public Class<String> getOutputType() {
		return String.class;
	}

	public String getOutputValue(StringPair val) {
		return val.getGenerated();
	}

	public boolean isLegal(StringPair val) {
		return true;
	}

	public String toString(StringPair val) {
		return val.getGenerated();
	}

	protected class MatchTaskDelegate extends
			AbstractTaskDelegate<List<MatchPair<StringPair>>> {

		protected List<String> tokens;

		public MatchTaskDelegate(List<String> tokens) {
			this.tokens = tokens;
		}

		@Override
		public void execute() {
			List<MatchPair<StringPair>> out = new ArrayList<MatchPair<StringPair>>();
			StringBuffer buffer = new StringBuffer();
			boolean first = true;
			for(String token : tokens) {
				if (cancelled)
					return;
				if (first)
					first = false;
				else
					buffer.append(" ");
				buffer.append(token);
			}
			String str = buffer.toString();
			List<String> newTokens = Collections.singletonList(str);
			if (cancelled)
				return;
			List<StringPair> pairs = getDisplayValues(buffer.toString());
			if (cancelled)
				return;
			for(StringPair pair : pairs) {
				if (cancelled)
					return;
				String s = pair.getGenerated();
				Map<String, int[]> hits = StringUtil.getMatchMap(s, newTokens,
						true, isIgnoreCase());
				if (cancelled)
					return;
				if (hits.size() > 0) {
					double weight = getWeight(pair);
					MatchPair<StringPair> mp = new MatchPair<StringPair>(pair, s, newTokens,
							weight, hits);
					out.add(mp);
				}
			}
			setResults(out);
		}
	}
	
	protected boolean isIgnoreCase() {
		return true;
	}

	public TaskDelegate<List<MatchPair<StringPair>>> getObjects(
			List<String> tokens) {
		return new MatchTaskDelegate(tokens);
	}

	public double getWeight(StringPair val) {
		return 1;
	}
}
