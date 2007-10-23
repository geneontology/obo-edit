package org.bbop.io;

import java.io.*;
import java.util.*;

public class MultiPrintStream extends PrintStream {

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

    public void print(boolean b) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(b);
	}
    }

    public void print(char c) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(c);
	}
    }

    public void print(char[] s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(s);
	}
    }

    public void print(double d) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(d);
	}
    }

    public void print(float f) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(f);
	}
    }

    public void print(int val) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(val);
	}
    }

    public void print(long l) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(l);
	}
    }

    public void print(Object obj) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(obj);
	}
    }

    public void print(String s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.print(s);
	}
    }

// here

    public void println(boolean b) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(b);
	}
    }

    public void println(char c) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(c);
	}
    }

    public void println(char[] s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(s);
	}
    }

    public void println(double d) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(d);
	}
    }

    public void println(float f) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(f);
	}
    }

    public void println(int val) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(val);
	}
    }

    public void println(long l) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(l);
	}
    }

    public void println(Object obj) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(obj);
	}
    }

    public void println(String s) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.println(s);
	}
    }

    public void write(int b) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.write(b);
	}
    }

    public void write(byte[] b, int off, int len) {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.write(b, off, len);
	}
    }

    public void close() {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.close();
	}
    }

    public void flush() {
	for(int i=0; i < streamList.size(); i++) {
	    PrintStream stream = (PrintStream) streamList.get(i);
	    stream.flush();
	}
    }
}
