package org.bbop.io;

import java.io.*;
import java.util.*;

public class MultiOutputStream extends OutputStream {

    Vector streamList;

    public MultiOutputStream() {
	streamList = new Vector();
    }

    public void addOutputStream(OutputStream stream) {
	streamList.addElement(stream);
    }

    public void removeOutputStream(OutputStream stream) {
	streamList.removeElement(stream);
    }

    public void write(int b) throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.write(b);
	}
    }

    public void write(byte[] b) throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.write(b);
	}
    }

    public void write(byte[] b, int off, int len) throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.write(b, off, len);
	}
    }

    public void close() throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.close();
	}
    }

    public void flush() throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.flush();
	}
    }
}
