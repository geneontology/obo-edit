package org.bbop.io;

import java.io.*;
import java.util.*;

import org.apache.log4j.*;

public class MultiPrintStream extends PrintStream {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiPrintStream.class);

    List streamList;

    public MultiPrintStream() {
	super(new ByteArrayOutputStream(0));
	streamList = new LinkedList();
    }

    public void addPrintStream(PrintStream stream) {
	streamList.add(stream);
    }

    public void removePrintStream(PrintStream stream) {
	streamList.remove(stream);
    }

    @Override
	public void print(boolean b) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(b);
	}
    }

    @Override
	public void print(char c) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(c);
	}
    }

    @Override
	public void print(char[] s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(s);
	}
    }

    @Override
	public void print(double d) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(d);
	}
    }

    @Override
	public void print(float f) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(f);
	}
    }

    @Override
	public void print(int val) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(val);
	}
    }

    @Override
	public void print(long l) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(l);
	}
    }

    @Override
	public void print(Object obj) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(obj);
	}
    }

    @Override
	public void print(String s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(s);
	}
    }

// here

    @Override
	public void println(boolean b) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(b);
	}
    }

    @Override
	public void println(char c) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(c);
	}
    }

    @Override
	public void println(char[] s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(s);
	}
    }

    @Override
	public void println(double d) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(d);
	}
    }

    @Override
	public void println(float f) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(f);
	}
    }

    @Override
	public void println(int val) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(val);
	}
    }

    @Override
	public void println(long l) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(l);
	}
    }

    @Override
	public void println(Object obj) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(obj);
	}
    }

    @Override
	public void println(String s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(s);
	}
    }

    @Override
	public void write(int b) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.write(b);
	}
    }

    @Override
	public void write(byte[] b, int off, int len) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.write(b, off, len);
	}
    }

    @Override
	public void close() {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.close();
	}
    }

    @Override
	public void flush() {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.flush();
	}
    }
}
