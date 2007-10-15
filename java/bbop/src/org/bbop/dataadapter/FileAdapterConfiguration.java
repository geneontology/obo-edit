package org.bbop.dataadapter;

import java.util.*;

/**
 *
 * An implementation of AdapterConfiguration for adapters that reads
 * one or more files and write a single file
 * 
 */
public class FileAdapterConfiguration implements AdapterConfiguration {

    protected int maxReadHistorySize;
    protected int maxWriteHistorySize;
    protected Collection<String> readPaths = new Vector<String>();
    protected String writePath;

    public FileAdapterConfiguration(String readFile) {
	this();
	readPaths.add(readFile);
    }

    public FileAdapterConfiguration() {
    }

    public Collection<String> getReadPaths() {
	return readPaths;
    }

    public void setReadPaths(Collection<String> c) {
	this.readPaths = c;
    }

    public void setWritePath(String path) {
	this.writePath = path;
    }

    public String getWritePath() {
	return writePath;
    }

    public String toString() {
	return "readPaths = "+readPaths;
    }
}
