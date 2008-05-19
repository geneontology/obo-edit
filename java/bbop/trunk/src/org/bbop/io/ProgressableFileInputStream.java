package org.bbop.io;

import java.io.*;

import org.apache.log4j.*;

public class ProgressableFileInputStream extends ProgressableInputStream {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ProgressableFileInputStream.class);

    public ProgressableFileInputStream(File in) throws FileNotFoundException {
	stream = new FileInputStream(in);
	fileSize = in.length();
    }

    public ProgressableFileInputStream(String name) throws FileNotFoundException {
	this(new File(name));
    }
}
