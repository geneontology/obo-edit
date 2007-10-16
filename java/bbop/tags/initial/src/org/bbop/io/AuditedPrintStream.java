package org.bbop.io;

import java.io.*;

public class AuditedPrintStream extends PrintStream {

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

    public boolean checkError() {
	return stream.checkError();
    }

    public void close() {
	stream.close();
    }

    public void flush() {
	stream.flush();
    }

    public void print(boolean b) {
	stream.print(b);
    }

    public void print(char c) {
	stream.print(c);
    }

    public void print(char[] s) {
	stream.print(s);
    }

    public void print(double d) {
	stream.print(d);
    }

    public void print(float f) {
	stream.print(f);
    }

    public void print(int i) {
	stream.print(i);
    }

    public void print(long l) {
	stream.print(l);
    }

    public void print(Object obj) {
	stream.print(obj);
    }

    public void print(String s) {
	stream.print(s);
    }

    public void println() {
	printLeader();
	stream.println();
    }

    public void println(boolean x) {
	printLeader();
	stream.println(x);
    }

    public void println(char x) {
	printLeader();
	stream.println(x);
    }

    public void println(char[] x) {
	printLeader();
	stream.println(x);
    }

    public void println(double x) {
	printLeader();
	stream.println(x);
    }

    public void println(float x) {
	printLeader();
	stream.println(x);
    }

    public void println(int x) {
	printLeader();
	stream.println(x);
    }

    public void println(long x) {
	printLeader();
	stream.println(x);
    }

    public void println(Object x) {
	printLeader();
	stream.println(x);
    }

    public void println(String x) {
	printLeader();
	stream.println(x);
    }

    protected void setError() {
//	stream.setError();
    }

    public void write(byte[] buf, int off, int len) {
	stream.write(buf, off, len);
    }
    
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
