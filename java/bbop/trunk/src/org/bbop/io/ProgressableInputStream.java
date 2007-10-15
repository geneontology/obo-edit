package org.bbop.io;

import java.io.*;
import java.util.Vector;

import org.bbop.util.*;

public abstract class ProgressableInputStream extends InputStream implements
		ProgressValued {

	protected long fileSize = 0;
	protected long currentPos = 0;
	protected double currentProgress = 0.0d;
	protected double percentInc = 1.0;
	protected Vector listeners = new Vector();
	protected String progressMessage = "Reading...";

	protected InputStream stream;

	public String getProgressString() {
		return progressMessage;
	}

	public Number getProgressValue() {
		return 100 * ((double) currentPos) / (fileSize);
	}

	protected void setStream(InputStream stream) {
		this.stream = stream;
	}

	protected void setStreamSize(long fileSize) {
		this.fileSize = fileSize;
	}

	public void setPercentIncrement(double percent) {
		percentInc = percent;
	}

	public double getPercentIncremement() {
		return percentInc;
	}

	public void setProgressMessage(String message) {
		this.progressMessage = message;
	}

	public int read() throws IOException {
		currentPos = currentPos + 1;
		return stream.read();
	}

	public int read(byte[] bytes) throws IOException {
		int amtread = stream.read(bytes);
		currentPos += amtread;
		return amtread;
	}

	public int read(byte[] bytes, int off, int len) throws IOException {
		int amtread = stream.read(bytes, off, len);
		currentPos += amtread;
		return amtread;
	}

	public long skip(long amt) throws IOException {
		long amtskipped = stream.skip(amt);
		currentPos += amtskipped;
		return amtskipped;
	}

	public int available() throws IOException {
		return stream.available();
	}

	public void close() throws IOException {
		stream.close();
	}

	public void mark(int readLimit) {
		stream.mark(readLimit);
	}

	public void reset() throws IOException {
		stream.reset();
	}

	public boolean markSupported() {
		return stream.markSupported();
	}
}
