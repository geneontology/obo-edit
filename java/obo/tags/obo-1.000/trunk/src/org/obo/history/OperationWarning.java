package org.obo.history;

import java.util.*;

public class OperationWarning {

	protected String message;
	protected int level;
	protected Vector subwarnings = new Vector();

	public static int DANGEROUS = 0;
	public static int FAILED = 1;

	public OperationWarning(String message) {
		this(message, FAILED);
	}

	public OperationWarning(String message, int level) {
		this.message = message;
		this.level = level;
	}

	public void addWarning(OperationWarning warning) {
		if (warning.failed())
			level = FAILED;
		subwarnings.add(warning);
	}

	public boolean failed() {
		return level == FAILED;
	}

	public String getMessage() {
		return getMessage(0);
	}

	protected String getMessage(int indentLevel) {
		StringBuffer out = new StringBuffer();
		if (indentLevel > 0)
			out.append("\n");
		for (int i = 0; i < indentLevel * 2; i++)
			out.append(" ");
		out.append(message);
		for (int i = 0; i < subwarnings.size(); i++) {
			OperationWarning ow = (OperationWarning) subwarnings.get(i);
			out.append(ow.getMessage(indentLevel + 1));
		}
		return out.toString();
	}

	@Override
	public String toString() {
		return "[Operation Warning] " + getMessage();
	}
}
