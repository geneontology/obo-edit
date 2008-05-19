package org.bbop.util;

import java.io.*;

import org.apache.log4j.*;

public class ProcessCommunicator extends Thread {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ProcessCommunicator.class);

	protected Process p;

	protected StringBuffer stdoutBuffer;
	protected StringBuffer stderrBuffer;

	protected String cmd;
	protected String[] args;
	protected static final String sep = System.getProperty("line.separator");

	protected boolean isRunning = false;

	protected static class ReaderRunnable implements Runnable {
		protected InputStream stream;
		protected StringBuffer buffer;
		protected boolean halt;
		protected Exception failedException;

		public ReaderRunnable(InputStream stream, StringBuffer buffer) {
			this.stream = stream;// new BufferedInputStream(stream);
			this.buffer = buffer;
		}

		public Exception getFailedException() {
			return failedException;
		}

		public void run() {
			try {

				int read;

				while ((read = stream.read()) != -1) {
					if (halt)
						return;
					buffer.append((char) read);
				}
			} catch (Exception ex) {
				if (halt)
					return;
				failedException = ex;
			}
		}

		public void halt() {
			halt = true;
			try {
				stream.close();
			} catch (IOException ex) {
			}
		}
	}

	protected Thread stdoutThread;
	protected ReaderRunnable stdoutRunnable;

	protected Thread stderrThread;
	protected ReaderRunnable stderrRunnable;

	protected Exception failureException;

	public String getCmd() {
		return cmd;
	}

	public ProcessCommunicator(String cmd) throws IOException {
		this.cmd = cmd;
		stdoutBuffer = new StringBuffer();
		stderrBuffer = new StringBuffer();
	}

	public ProcessCommunicator(String[] args) throws IOException {
		this.args = args;
		stdoutBuffer = new StringBuffer();
		stderrBuffer = new StringBuffer();
	}

	public void run() {
		try {
			p.waitFor();
			isRunning = false;
		} catch (Exception ex) {
			isRunning = false;
			failureException = ex;
		}
	}

	protected void setupThread() throws Exception {
		if (args == null)
			this.p = Runtime.getRuntime().exec(cmd);
		else
			this.p = Runtime.getRuntime().exec(args);
		stdoutRunnable = new ReaderRunnable(p.getInputStream(), stdoutBuffer);
		stderrRunnable = new ReaderRunnable(p.getErrorStream(), stderrBuffer);

		stdoutThread = new Thread(stdoutRunnable);
		stderrThread = new Thread(stderrRunnable);
		stdoutThread.start();
		stderrThread.start();
		isRunning = true;
	}

	public void start() {
		try {
			setupThread();
			super.start();
		} catch (Exception ex) {
			isRunning = false;
			failureException = ex;
		}
	}

	public Exception getFailureException() {
		return failureException;
	}

	public int exitValue() {
		if (isRunning())
			throw new IllegalArgumentException(
					"Can't call exitValue() while process is running");
		else
			return p.exitValue();
	}

	public ProcessCommunicator getCleanCopy() throws IOException {
		if (cmd != null)
			return new ProcessCommunicator(cmd);
		else
			return new ProcessCommunicator(args);
	}

	public void shutdown() {
		isRunning = false;
		stderrRunnable.halt();
		stdoutRunnable.halt();
		p.destroy();
	}

	public boolean isRunning() {
		return isRunning || (stdoutThread != null && stdoutThread.isAlive())
				|| (stderrThread != null && stderrThread.isAlive());
		// return isRunning;
	}

	public boolean isReadable() {
		return isRunning() || stdoutBuffer.length() > 0
				|| stderrBuffer.length() > 0;
	}

	public String readStdoutLine() {
		return readStdoutLine(true);
	}

	public String readStderrLine() {
		return readStderrLine(true);
	}

	public String readStdoutLine(boolean block) {
		return readLine(stdoutBuffer, block);
	}

	public String readStderrLine(boolean block) {
		return readLine(stderrBuffer, block);
	}

	public String readStdoutBuffer() {
		return readBuffer(stdoutBuffer);
	}

	public String readStderrBuffer() {
		return readBuffer(stderrBuffer);
	}

	/**
	 * Use this command at your peril! It may deadlock or worse if the process
	 * requires input!
	 */
	public static class ProcessResult {
		protected int exitCode;
		protected String stdout;
		protected String stderr;

		protected ProcessResult(int exitCode, String stdout, String stderr) {
			this.exitCode = exitCode;
			this.stdout = stdout;
			this.stderr = stderr;
		}

		public String getStdout() {
			return stdout;
		}

		public String getStderr() {
			return stderr;
		}

		public int getExitCode() {
			return exitCode;
		}
	}

	public static ProcessResult run(String cmd) throws Exception {
		StringBuffer err = new StringBuffer();
		StringBuffer out = new StringBuffer();
		ProcessCommunicator pc = new ProcessCommunicator(cmd);
		pc.start();
		while (pc.isReadable()) {
			String line;
			while ((line = pc.readStderrLine(false)) != null) {
				err.append(line);
				err.append(sep);
			}
			while ((line = pc.readStdoutLine(false)) != null) {
				out.append(line);
				out.append(sep);
			}
			Thread.yield();
		}
		if (pc.getFailureException() != null)
			throw pc.getFailureException();
		return new ProcessResult(pc.exitValue(), out.toString(), err.toString());
	}

	protected String readBuffer(StringBuffer buffer) {
		if (buffer.length() == 0)
			return null;
		int length = buffer.length();
		String out = buffer.substring(0, length);
		buffer.delete(0, length);
		return out;
	}

	protected String readLine(StringBuffer buffer, boolean block) {
		if (block) {
			String line;
			while ((line = readLine(buffer)) == null) {
				if (!isRunning())
					return null;
				Thread.yield();
			}
			return line;
		} else
			return readLine(buffer);
	}

	protected String readLine(StringBuffer buffer) {
		if (buffer.length() == 0)
			return null;
		int i = buffer.indexOf(sep);
		if (i == -1) {
			if (!isRunning()) {
				return readBuffer(buffer);
			} else
				return null;
		}
		String out = buffer.substring(0, i);
		buffer.delete(0, i + sep.length());
		return out;
	}

	public void finalize() {
		p.destroy();
	}

	public void println(String s) throws IOException {
		OutputStream stream = p.getOutputStream();
		for (int i = 0; i < s.length(); i++)
			stream.write(s.charAt(i));
		stream.flush();
	}

}
