package org.bbop.io;

import java.io.*;
import java.util.*;

import org.apache.log4j.*;

public class MultiOutputStream extends OutputStream {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiOutputStream.class);

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

    @Override
	public void write(int b) throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.write(b);
	}
    }

    @Override
	public void write(byte[] b) throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.write(b);
	}
    }

    @Override
	public void write(byte[] b, int off, int len) throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.write(b, off, len);
	}
    }

    @Override
	public void close() throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.close();
	}
    }

    @Override
	public void flush() throws IOException {
	for(int i=0; i < streamList.size(); i++) {
	    OutputStream stream = (OutputStream) streamList.elementAt(i);
	    stream.flush();
	}
    }
}
