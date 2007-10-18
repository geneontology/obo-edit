package org.obo.query.impl;

public class ScoredStringHit {

	protected String userString;

	protected String matchString;

	protected double score;

	protected int hitPos;

	public ScoredStringHit(String userString, String matchString,
			double score, int hitPos) {
		this.userString = userString;
		this.matchString = matchString;
		this.score = score;
		this.hitPos = hitPos;
	}

	public int getHitPos() {
		return hitPos;
	}

	public String getMatchString() {
		return matchString;
	}

	public double getScore() {
		return score;
	}

	public String getUserString() {
		return userString;
	}

}
