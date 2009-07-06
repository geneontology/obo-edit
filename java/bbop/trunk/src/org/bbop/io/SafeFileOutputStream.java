package org.bbop.io;

import java.io.*;

import org.apache.log4j.*;

public class SafeFileOutputStream extends OutputStream {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SafeFileOutputStream.class);

    protected File tempFile;
    protected File file;
    protected FileOutputStream stream;
    protected boolean deleteOnFail;

    public SafeFileOutputStream(String path) throws IOException {
	this(path, true);
    }

    public SafeFileOutputStream(File file) throws IOException {
	this(file, true);
    }

    public SafeFileOutputStream(String path, boolean deleteOnFail) throws
	IOException {
	this(new File(path));
    }

    public SafeFileOutputStream(File file, boolean deleteOnFail)
	throws IOException {
	this.deleteOnFail = deleteOnFail;
	tempFile = File.createTempFile("temp", "output");
	stream = new FileOutputStream(tempFile);
	tempFile.deleteOnExit();
	this.file = file;
    }

    @Override
	public void flush() throws IOException {
	try {
	    stream.flush();
	} catch (IOException ex) {
	    tempFile.delete();
	    throw ex;
	} catch (RuntimeException ex) {
	    tempFile.delete();
	    throw ex;
	}
    }

    public void fail() throws IOException {
	stream.close();
	if (deleteOnFail)
	    tempFile.delete();
    }

    public File getTempFile() {
	return tempFile;
    }

    @Override
	public void finalize() throws IOException {
	fail();
    }

    @Override
	public void write(byte[] b) throws IOException {
	try {
	    stream.write(b);
	} catch (IOException ex) {
	    tempFile.delete();
	    throw ex;
	} catch (RuntimeException ex) {
	    tempFile.delete();
	    throw ex;
	}
    }

    @Override
	public void write(byte[] b, int off, int len) throws IOException {
	try {
	    stream.write(b, off, len);
	} catch (IOException ex) {
	    tempFile.delete();
	    throw ex;
	} catch (RuntimeException ex) {
	    tempFile.delete();
	    throw ex;
	}
    }

    @Override
	public void write(int b) throws IOException {
	try {
	    stream.write(b);
	} catch (IOException ex) {
	    tempFile.delete();
	    throw ex;
	} catch (RuntimeException ex) {
	    tempFile.delete();
	    throw ex;
	}
    }

    @Override
	public void close() throws IOException {
	try {
	    stream.close();
	    file.delete();
	    IOUtil.copyFile(tempFile, file);
	    tempFile.delete();
	} catch (IOException ex) {
	    ex.printStackTrace();
	    tempFile.delete();
	    throw ex;
	} catch (RuntimeException ex) {
	    ex.printStackTrace();
	    tempFile.delete();
	    throw ex;
	}
    }
}
