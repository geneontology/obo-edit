package org.obo.dataadapter;

import org.bbop.dataadapter.*;
import org.bbop.util.StringUtil;

import java.net.URL;

import org.apache.log4j.*;

public class GOFlatFileParseException extends DataAdapterException {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GOFlatFileParseException.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected int lineNumber;
	protected int colNumber;
	protected String line;
	protected String filename;

	protected GOFlatFileParseException(String message) {
		super(message);
	}

	public GOFlatFileParseException(String message, String filename,
			String line, int lineNumber, int colNumber) {
		super(message);
		this.filename = filename;
		this.line = line;
		this.lineNumber = lineNumber;
		this.colNumber = colNumber;
	}

	public GOFlatFileParseException(String message, URL filename, String line,
			int lineNumber, int colNumber) {
		this(message, filename.toString(), line, lineNumber, colNumber);
	}

	public int getLineNumber() {
		return lineNumber;
	}

	public int getColNumber() {
		return colNumber;
	}

	public String getLine() {
		return line;
	}

	public String getFilename() {
		return filename;
	}

	@Override
	public String toString() {
		return filename + ":" + lineNumber + ": " + getMessage() + "\n" + line
				+ "\n" + StringUtil.repeat(' ', colNumber - 1) + "^";
	}
}
