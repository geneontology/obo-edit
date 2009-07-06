package org.bbop.io;

import java.io.*;

import org.apache.log4j.*;

public class AuditedPrintStream extends PrintStream {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AuditedPrintStream.class);

    protected int padding;
    protected PrintStream stream;
    protected boolean suppressStackTraces;

    public AuditedPrintStream(PrintStream stream) {
	this(stream, 0, false);
    }

    public AuditedPrintStream(PrintStream stream, int padding,
			      boolean suppressStackTraces) {
	super(stream);
	this.stream = stream;
	this.padding = padding;
	this.suppressStackTraces = suppressStackTraces;
    }

    @Override
	public boolean checkError() {
	return stream.checkError();
    }

    @Override
	public void close() {
	stream.close();
    }

    @Override
	public void flush() {
	stream.flush();
    }

    @Override
	public void print(boolean b) {
	stream.print(b);
    }

    @Override
	public void print(char c) {
	stream.print(c);
    }

    @Override
	public void print(char[] s) {
	stream.print(s);
    }

    @Override
	public void print(double d) {
	stream.print(d);
    }

    @Override
	public void print(float f) {
	stream.print(f);
    }

    @Override
	public void print(int i) {
	stream.print(i);
    }

    @Override
	public void print(long l) {
	stream.print(l);
    }

    @Override
	public void print(Object obj) {
	stream.print(obj);
    }

    @Override
	public void print(String s) {
	stream.print(s);
    }

    @Override
	public void println() {
	printLeader();
	stream.println();
    }

    @Override
	public void println(boolean x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(char x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(char[] x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(double x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(float x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(int x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(long x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(Object x) {
	printLeader();
	stream.println(x);
    }

    @Override
	public void println(String x) {
	printLeader();
	stream.println(x);
    }

    @Override
	protected void setError() {
//	stream.setError();
    }

    @Override
	public void write(byte[] buf, int off, int len) {
	stream.write(buf, off, len);
    }
    
    @Override
	public void write(int b) {
	stream.write(b);
    }

    protected StackTraceElement getFirstInterestingElement(Exception e) {
	StackTraceElement [] elts = e.getStackTrace();
	for(int i=0; i < elts.length; i++) {
	    Class c = null;
	    try {
		c = Class.forName(elts[i].getClassName());
	    } catch (Exception ex) {}
	    if (c != null) {
		if (!Exception.class.isAssignableFrom(c) &&
		    !OutputStream.class.isAssignableFrom(c))
		    return elts[i];
	    }
	}
	return null;
    }

    protected void printLeader() {
	Exception e = new Exception();
	StackTraceElement elt = getFirstInterestingElement(e);
//	e.printStackTrace(System.out);
	if (elt != null &&
	    elt.getFileName() != null &&
	    (!elt.getFileName().equals("Throwable.java") ||
	     !suppressStackTraces)) {
	    StringBuffer buffer = new StringBuffer();
	    buffer.append(elt.getFileName());
	    buffer.append(':');
	    buffer.append(elt.getLineNumber());
	    buffer.append(':');
	    for(int i=buffer.length(); i < padding; i++) {
		buffer.append(' ');
	    }
	    stream.print(buffer.toString());
	}
    }
}
