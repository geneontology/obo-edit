package org.obo.dataadapter;

public class GOToken {

	protected String token;
	protected String line;
	protected int colLoc;
	protected int lineNum;
	protected String filename;
	protected boolean flush;

	public GOToken() {
	}

	public GOToken(String token, String filename, String line, int lineNum,
			int colLoc, boolean flush) {
		setStrings(token, filename, line, lineNum, colLoc, flush);
	}

	public void setStrings(String token, String filename, String line,
			int lineNum, int colLoc, boolean flush) {
		this.filename = filename;
		this.token = token;
		this.colLoc = colLoc;
		this.lineNum = lineNum;
		this.line = line;
		this.flush = flush;
	}

	public String getFilename() {
		return filename;
	}

	public boolean isFlush() {
		return flush;
	}

	public String getToken() {
		return token;
	}

	public int getColNumber() {
		return colLoc;
	}

	public int getLineNumber() {
		return lineNum;
	}

	public String getLine() {
		return line;
	}

	@Override
	public String toString() {
		return "file = " + filename + "\n" + "linenum = " + lineNum + "\n"
				+ "colnum = " + colLoc + "\n" + "token = \"" + token + "\"\n"
				+ "flush = " + flush + "\n";
	}
}
