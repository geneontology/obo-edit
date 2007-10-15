package org.bbop.io;

import java.io.*;

public class ProgressableFileInputStream extends ProgressableInputStream {

    public ProgressableFileInputStream(File in) throws FileNotFoundException {
	stream = new FileInputStream(in);
	fileSize = in.length();
    }

    public ProgressableFileInputStream(String name) throws FileNotFoundException {
	this(new File(name));
    }
}
