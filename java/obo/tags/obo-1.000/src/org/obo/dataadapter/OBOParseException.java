package org.obo.dataadapter;

public class OBOParseException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected int linenum;
	protected int charnum;
	protected String line;
	protected String filename;

	public OBOParseException(String message, String filename, String line,
			int linenum, int charnum) {
		super(message);
		this.line = line;
		this.filename = filename;
		this.linenum = linenum;
		this.charnum = charnum;
	}

	public String getPath() {
		return filename;
	}

	public OBOParseException(String message, String path, String line,
			int linenum) {
		this(message, path, line, linenum, -1);
	}

	public String getLine() {
		return line;
	}

	public int getLineNum() {
		return linenum;
	}

	public int getCharNum() {
		return charnum;
	}

	@Override
	public String toString() {
		return "Error: " + getMessage() + "\n" + "on line: " + linenum
				+ (filename != null ? " of " + filename : "") + "\ncharnum: "
				+ charnum + "\nline: " + line;
	}
}
