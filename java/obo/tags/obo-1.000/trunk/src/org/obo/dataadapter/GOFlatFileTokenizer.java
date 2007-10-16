package org.obo.dataadapter;

import java.net.URL;

public class GOFlatFileTokenizer {

	private static final byte SKIP = 1;
	private static final byte BOUNDARY = 2;
	private static final byte TOKEN = 4;
	private static final int INIT_ESC_BUFFER_SIZE = 1000;
	protected char[] escBuffer = new char[INIT_ESC_BUFFER_SIZE];

	public static final char escapeChar = '\\';
	String parseme;
	int currentloc;
	int lineNum;
	String filename;

	protected byte[] charstatus = new byte[Character.MAX_VALUE + 1];

	public GOFlatFileTokenizer(URL filename, String parseme, int lineNum) {
		this(filename.toString(), parseme, lineNum);
	}

	public GOFlatFileTokenizer(String filename, String parseme, int lineNum) {
		setStrings(filename, parseme, lineNum);
	}

	public GOFlatFileTokenizer() {
	}

	public void setStrings(String filename, String parseme, int lineNum) {
		this.filename = filename;
		this.parseme = parseme;
		this.lineNum = lineNum;
		currentloc = 0;
	}

	protected boolean checkStatus(char c, byte status) {
		return (charstatus[c] & status) > 0;
	}

	protected boolean skipGarbage() {
		int loc = currentloc;
		for (; loc < parseme.length(); loc++) {
			if (!checkStatus(parseme.charAt(loc), SKIP))
				break;
		}
		boolean skippedSomething = (currentloc != loc);
		currentloc = loc;
		return skippedSomething;
	}

	public GOToken getNextToken() throws GOFlatFileParseException {
		boolean flush = !skipGarbage();
		int loc = currentloc;
		int start = currentloc;

		final int parsemelength = parseme.length();

		boolean boundaryActive = false;
		char boundaryChar = '\0';

		if (loc >= parsemelength)
			return null;
		for (; loc < parsemelength; loc++) {
			char c = parseme.charAt(loc);
			if (c == escapeChar) {
				loc = loc + 1;
				if (loc >= parsemelength)
					throw new GOFlatFileParseException(
							"Illegal escape character at end of line",
							filename, parseme, lineNum, parseme.length() - 1);
				continue;
			}

			if ((charstatus[c] & SKIP) > 0) {
				currentloc = loc;
				break;
			}

			if ((charstatus[c] & TOKEN) > 0) {
				if (start == loc) {
					currentloc = loc + 1;
					return new GOToken(c + "", filename, parseme, lineNum,
							loc + 1, flush);
				} else {
					currentloc = loc;
					break;
				}
			}

			if (boundaryActive && boundaryChar == c) {
				loc = loc + 1;
				currentloc = loc;
				break;
			}

			if ((charstatus[c] & BOUNDARY) > 0) {
				boundaryChar = c;
				boundaryActive = true;
			}
		}
		if (loc >= parsemelength) {
			currentloc = loc + 1;
		}

		return new GOToken(unescape(parseme, start, loc), filename, parseme,
				lineNum, start + 1, flush);
	}

	public String unescape(String in, int start, int end) {
		int length = 0;
		for (int i = start; i < end; i++) {
			char c = in.charAt(i);
			if (c == escapeChar)
				c = in.charAt(++i);
			insert(length, c);
			length++;
		}
		return new String(escBuffer, 0, length);
	}

	protected void insert(int loc, char c) {
		if (loc >= escBuffer.length) {
			char[] newBuffer = new char[escBuffer.length * 2];
			for (int i = 0; i < escBuffer.length; i++)
				newBuffer[i] = escBuffer[i];
			escBuffer = newBuffer;
		}
		escBuffer[loc] = c;
	}

	public void addTokenChar(char ch) {
		charstatus[ch] = (byte) (charstatus[ch] | SKIP);
	}

	public void addBoundaryChar(char ch) {
		charstatus[ch] = (byte) (charstatus[ch] | BOUNDARY);
	}

	public void addKeeperTokenChar(char ch) {
		charstatus[ch] = (byte) (charstatus[ch] | TOKEN);
	}
}
