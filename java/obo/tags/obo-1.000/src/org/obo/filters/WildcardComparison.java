package org.obo.filters;

import java.util.*;
import java.util.regex.*;

public class WildcardComparison extends AbstractComparison {

	protected Class[] types = { String.class };
	protected Map patternMap = new HashMap();

	@Override
	public void init() {
		patternMap.clear();
	}

	@Override
	public void cleanup() {
		patternMap.clear();
	}

	public Class[] getAcceptedTypes() {
		return types;
	}

	public String getID() {
		return "*=";
	}

	protected Pattern getPattern(String value) {
		Pattern pattern = (Pattern) patternMap.get(value);
		if (pattern == null) {
			pattern = Pattern.compile(getRegexp(value),
					Pattern.CASE_INSENSITIVE);
			patternMap.put(value, pattern);
		}
		return pattern;
	}

	public String getRegexp(String val) {
		boolean escaped = false;
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < val.length(); i++) {
			char c = val.charAt(i);
			if (escaped) {
				escaped = false;
				if (c == '\\' || c == '*')
					out.append(c);
				else {
					out.append('\\');
					out.append(c);
				}
			} else if (c == '\\')
				escaped = true;
			else if (c == '*') {
				out.append(".*");
			} else
				out.append(c);
		}
		return out.toString();
	}

	public boolean matches(Collection testVals, String value) {
		if (value == null)
			value = "";
		Pattern p = getPattern(value);

		Iterator it = testVals.iterator();
		while (it.hasNext()) {
			String s = it.next().toString();
			Matcher m = p.matcher(s);
			if (m.matches())
				return true;
		}
		return false;
	}

	@Override
	public String toString() {
		return "matches wildcard";
	}
}
