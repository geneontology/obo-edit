package org.bbop.io;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Locale;

public abstract class AbstractExtendedPrintStream extends PrintStream {

	protected ByteArrayOutputStream stream;

	public AbstractExtendedPrintStream() {
		this(new ByteArrayOutputStream());
	}

	protected AbstractExtendedPrintStream(ByteArrayOutputStream stream) {
		super(stream);
		this.stream = stream;
	}

	@Override
	public PrintStream format(Locale l, String format, Object... args) {
		super.format(l, format, args);
		callWrite();
		return this;
	}

	@Override
	public void print(boolean b) {
		super.print(b);
		callWrite();
	}

	@Override
	public void print(char c) {
		super.print(c);
		callWrite();
	}

	@Override
	public void print(char[] s) {
		super.print(s);
		callWrite();
	}
	
	@Override
	public void print(double d) {
		super.print(d);
		callWrite();
	}

	@Override
	public void print(float f) {
		super.print(f);
		callWrite();
	}
	
	@Override
	public void print(int i) {
		super.print(i);
		callWrite();
	}
	
	@Override
	public void print(long l) {
		super.print(l);
		callWrite();
	}
	
	@Override
	public void print(Object obj) {
		super.print(obj);
		callWrite();
	}
	
	@Override
	public void print(String s) {
		super.print(s);
		callWrite();
	}
	
	@Override
	public PrintStream printf(Locale l, String format, Object... args) {
		super.printf(l, format, args);
		callWrite();
		return this;
	}
	
	@Override
	public PrintStream printf(String format, Object... args) {
		super.printf(format, args);
		callWrite();
		return this;
	}
	
	@Override
	public void println() {
		super.println();
		callWrite();
	}
	
	@Override
	public void println(boolean x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(char x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(char[] x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(double x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(float x) {
		super.println(x);
		callWrite();
	}

	@Override
	public void println(int x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(long x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(Object x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void println(String x) {
		super.println(x);
		callWrite();
	}
	
	@Override
	public void flush() {
		callWrite();
	}
	
	protected void callWrite() {
		String s = stream.toString();
		stream.reset();
		writeString(s);
	}

	protected abstract void writeString(String s);
}
