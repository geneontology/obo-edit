/**
 * 
 */
package org.bbop.swing.autocomplete;

import java.util.List;
import java.util.Map;

import org.bbop.util.StringUtil;

public class MatchPair<DISPLAY_TYPE> {
	
	protected DISPLAY_TYPE val;
	protected Map<String, int[]> match;
	protected String str;
	protected List<String> tokens;
	protected double score;
	
	public MatchPair(DISPLAY_TYPE val, String str,
			List<String> tokens,
			double weight,
			Map<String, int[]> match) {
		super();
		this.val = val;
		this.match = match;
		this.str = str;
		score = StringUtil.score(str, tokens, match)*weight;
	}
	
	public double getScore() {
		return score;
	}
	
	public String getString() {
		return str;
	}

	public DISPLAY_TYPE getVal() {
		return val;
	}

	public Map<String, int[]> getMatch() {
		return match;
	}
	
	public String toString() {
		return "Matched "+str+" with "+val+" with score of "+score;
	}
}