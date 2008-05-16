package org.oboedit.verify;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.IdentifiedObject;

import org.apache.log4j.*;

public class TextCheckWarning extends CheckWarning {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextCheckWarning.class);

	protected int startIndex;

	protected FieldPath path;

	protected int endIndex;

	public TextCheckWarning(String message, boolean isFatal, Check source,
			int problemStartIndex, int problemEndIndex, FieldPath path) {
		this(message, isFatal, source, problemStartIndex, problemEndIndex,
				path, new LinkedList<QuickFix>(), null);
	}

	public TextCheckWarning(String message, boolean isFatal, Check source,
			IdentifiedObject object, int problemStartIndex, int problemEndIndex) {
		this(message, isFatal, source, problemStartIndex, problemEndIndex,
				new FieldPath(object), new LinkedList<QuickFix>(), null);
	}

	public TextCheckWarning(String message, boolean isFatal, Check source,
			int problemStartIndex, int problemEndIndex, FieldPath path,
			String type) {
		this(message, isFatal, source, problemStartIndex, problemEndIndex,
				path, new LinkedList<QuickFix>(), type);
	}

	public TextCheckWarning(String message, boolean isFatal, Check source,
			int problemStartIndex, int problemEndIndex, FieldPath path,
			Collection<QuickFix> fixes, String type) {
		super(message, isFatal, source, path, type, fixes);
		this.startIndex = problemStartIndex;
		this.endIndex = problemEndIndex;
	}

	public int getStartIndex() {
		return startIndex;
	}

	public int getEndIndex() {
		return endIndex;
	}
}
