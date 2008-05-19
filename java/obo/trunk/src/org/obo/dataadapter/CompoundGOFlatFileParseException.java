package org.obo.dataadapter;

import java.util.Vector;

import org.apache.log4j.*;

public class CompoundGOFlatFileParseException extends GOFlatFileParseException {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CompoundGOFlatFileParseException.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected final static int MAX_ERRORS = 100;
	Vector exceptions = new Vector();
	protected boolean empty;
	protected boolean hideDownstream;
	protected String message;

	public CompoundGOFlatFileParseException(boolean hideDownstream) {
		super("compound exception");
		this.hideDownstream = hideDownstream;
		empty = true;
	}

	public CompoundGOFlatFileParseException() {
		this(false);
	}

	public boolean isEmpty() {
		return empty;
	}

	public boolean isFull() {
		return exceptions.size() >= MAX_ERRORS;
	}

	public void addException(GOFlatFileParseException e)
			throws GOFlatFileParseException {
		if (empty) {
			lineNumber = e.getLineNumber();
			colNumber = e.getColNumber();
			line = e.getLine();
			message = e.getMessage();
			empty = false;
		}
		boolean ignoreException = false;
		if (exceptions.size() > 0) {
			GOFlatFileParseException last = (GOFlatFileParseException) exceptions
					.elementAt(exceptions.size() - 1);
			if (last.getLineNumber() == e.getLineNumber() && hideDownstream)
				ignoreException = true;
		}
		if (!ignoreException)
			exceptions.addElement(e);
		if (isFull())
			throw this;
	}

	@Override
	public String getMessage() {
		return message;
	}

	@Override
	public String toString() {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < exceptions.size(); i++) {
			out.append(exceptions.elementAt(i).toString() + "\n\n");
		}
		out.append(exceptions.size() + " errors");
		if (isFull())
			out.append("\n(note: there may be more errors; only " + MAX_ERRORS
					+ " errors are reported at a time)");
		return out.toString();
	}
}
