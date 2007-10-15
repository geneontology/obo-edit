package org.bbop.commandline;

import java.util.*;
import java.util.regex.*;

public class ValueSpec implements ArgumentSignature {

	protected String val;
	protected boolean lastResort = true;
	protected Pattern regexpPattern;
	protected String templateVal;

	protected int id = CommandLineParser.getID();

	public int getID() {
		return id;
	}

	public boolean equals(Object o) {
		if (o instanceof ArgumentSignature)
			return ((ArgumentSignature) o).getID() == id;
		else
			return false;
	}

	public int hashCode() {
		return id;
	}

	public String getShortDocumentation() {
		if (templateVal != null)
			return templateVal;
		else
			return "<value>";
	}

	public ValueSpec(String val) {
		this(val, false);
	}

	public ValueSpec(String val, boolean isRegexp) {
		if (isRegexp)
			regexpPattern = Pattern.compile(val);
		else
			templateVal = val;
	}

	public ValueSpec() {
		this(null);
	}

	protected ValueSpec(boolean internal) {
	}

	public String toString() {
		return "ValueSpec"
				+ id
				+ (regexpPattern != null ? "(" + regexpPattern.pattern() + ")"
						: "") + ":" + val;
	}

	public ArgumentSignature copy() {
		ValueSpec out = new ValueSpec(true);
		out.val = val;
		out.lastResort = lastResort;
		out.regexpPattern = regexpPattern;
		out.id = id;
		out.templateVal = templateVal;
		return out;
	}

	public void init(CommandLineParser p, boolean b) {
		val = null;
	}

	public void accept(CommandLineParser p) throws FailException {
		if (val == null) {
			String s = p.getNextString();
			if (regexpPattern != null) {
				if (regexpPattern.matcher(s).matches())
					val = s;
				else
					throw new FailException("Badly formatted value " + s);
			} else if (templateVal != null) {
				if (templateVal.equals(s))
					val = s;
				else
					throw new FailException("Expected " + templateVal
							+ " instead of " + s);
			} else if (s.startsWith("-")) {
				throw new FailException("Tag values can't start with -");
			} else {
				val = s;
			}
		} else
			throw new FailException(
					"UNEXPECTED CONDITION: Illegal attempt to replace " + val
							+ " with " + p.peekNextString());
	}

	public void setOnlyAcceptAsLastResort(boolean lastResort) {
		this.lastResort = lastResort;
	}

	public boolean onlyAcceptAsLastResort() {
		return lastResort;
	}

	public void fail() {
		val = null;
	}

	public List getValues() throws UnfullfilledException {
		if (val == null)
			throw new UnfullfilledException();
		return Collections.singletonList(new StringValue(val));
	}
}
