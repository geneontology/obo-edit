package org.obo.util;

import java.text.ParseException;
import java.util.Scanner;
import java.util.regex.MatchResult;

import org.bbop.util.StringUtil;

public class VersionNumber implements Comparable<VersionNumber> {

	protected int majorVersion;

	protected int minorVersion;

	protected int betaVersion;

	public VersionNumber(String str) throws ParseException {
		Scanner s = new Scanner(str);
		try {
			s.findInLine("(\\d+)");
			MatchResult result = s.match();
			majorVersion = Integer.parseInt(result.group(1));
		} catch (IllegalStateException ex) {
			throw new ParseException(str, -1);
		}
		try {
			s.findInLine(".(\\d+)");
			MatchResult result = s.match();
			minorVersion = Integer.parseInt(result.group(1));
		} catch (IllegalStateException ex) {
			throw new ParseException(str, -1);
		}
		try {
			s.findInLine("-beta(\\d+)");
			MatchResult result = s.match();
			betaVersion = Integer.parseInt(result.group(1));
		} catch (IllegalStateException ex) {
		}

		s.close();
	}

	public String toString() {
		return majorVersion + "."
				+ StringUtil.pad(minorVersion + "", '0', 3, true)
				+ (isBeta() ? "-beta" + betaVersion : "");
	}

	public static void main(String[] args) throws Exception {
		System.err.println(new VersionNumber("2.001-beta6"));
		System.err.println(new VersionNumber("3.008"));
	}

	public boolean isBeta() {
		return betaVersion > 0;
	}

	public int getBetaVersion() {
		return betaVersion;
	}

	public int getMajorVersion() {
		return majorVersion;
	}

	public int getMinorVersion() {
		return minorVersion;
	}

	public int hashCode() {
		return getMajorVersion() * 100000 + getMinorVersion() * 100
				+ getBetaVersion();
	}

	public boolean equals(Object o) {
		if (o instanceof VersionNumber) {
			VersionNumber vn = (VersionNumber) o;
			return vn.getBetaVersion() == getBetaVersion()
					&& vn.getMajorVersion() == getMajorVersion()
					&& vn.getMinorVersion() == getMinorVersion();
		} else
			return false;
	}

	public int compareTo(VersionNumber o) {
		int majorDiff = o.getMajorVersion() - getMajorVersion();
		int minorDiff = o.getMinorVersion() - getMinorVersion();
		int betaDiff = o.getBetaVersion() - getBetaVersion();
		return majorDiff * 100000 + minorDiff * 100 + betaDiff;
	}
}
