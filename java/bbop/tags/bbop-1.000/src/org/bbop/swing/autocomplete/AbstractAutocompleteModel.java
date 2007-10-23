package org.bbop.swing.autocomplete;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.StringUtil;
import org.bbop.util.TaskDelegate;

public abstract class AbstractAutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE>
		implements AutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE> {

	protected class MatchPairComparator implements
			Comparator<MatchPair<DISPLAY_TYPE>> {

		public int compare(MatchPair<DISPLAY_TYPE> o1,
				MatchPair<DISPLAY_TYPE> o2) {
			double val = o2.getScore() - o1.getScore();
			if (Math.abs(val) < 1 && val != 0)
				val = 1 / val;
			int intVal = (int) val;
			if (intVal == 0) {
				int lengthMatch = o1.getString().length() - o2.getString().length();
				if (lengthMatch == 0)
					return o1.getString().compareToIgnoreCase(o2.getString());
				else
					return lengthMatch;
			} else
				return intVal;
		}
	}

	protected MatchPairComparator matchComparator = new MatchPairComparator();

	protected class MatchTaskDelegate extends
			AbstractTaskDelegate<List<MatchPair<DISPLAY_TYPE>>> {

		protected List<String> tokens;

		public MatchTaskDelegate(List<String> tokens) {
			this.tokens = tokens;
		}

		@Override
		public void execute() {
			try {
				List<MatchPair<DISPLAY_TYPE>> out = new ArrayList<MatchPair<DISPLAY_TYPE>>();
				for (DISPLAY_TYPE obj : getAllValues()) {
					if (cancelled)
						return;
					String s = AbstractAutocompleteModel.this.toString(obj);
					Map<String, int[]> hits = StringUtil.getMatchMap(s, tokens,
							true);
					if (cancelled)
						return;
					if (hits.size() > 0) {
						double weight = getWeight(obj);
						MatchPair<DISPLAY_TYPE> mp = new MatchPair<DISPLAY_TYPE>(obj, s, tokens,
								weight, hits);
						out.add(mp);
					}
				}
				Collections.sort(out, matchComparator);
				if (cancelled)
					return;
				setResults(out);
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}
	}
	
	protected Collection<DISPLAY_TYPE> getDisplayValues(Collection<OUTPUT_TYPE> values) {
		Collection<DISPLAY_TYPE> out = new HashSet<DISPLAY_TYPE>();
		for(OUTPUT_TYPE o : values) {
			out.addAll(getDisplayValues(o));
		}
		return out;
	}
	
	protected Collection<OUTPUT_TYPE> getOutputValues(Collection<DISPLAY_TYPE> values) {
		Collection<OUTPUT_TYPE> out = new HashSet<OUTPUT_TYPE>();
		for(DISPLAY_TYPE d : values) {
			out.add(getOutputValue(d));
		}
		return out;
	}

	public TaskDelegate<List<MatchPair<DISPLAY_TYPE>>> getObjects(
			List<String> tokens) {
		return new MatchTaskDelegate(tokens);
	}

	public double getWeight(DISPLAY_TYPE val) {
		return 1;
	}

}
