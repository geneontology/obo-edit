package org.obo.filters;

public class RegexpComparison extends WildcardComparison {

	@Override
	public String getID() {
		return "~=";
	}

	@Override
	public String getRegexp(String val) {
		return val;
	}

	@Override
	public String toString() {
		return "matches regexp";
	}
}
