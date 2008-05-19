package org.bbop.io;

import java.io.*;

import org.apache.log4j.*;

public class AllFileFilter implements FilenameFilter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllFileFilter.class);

    public boolean accept(File dir, String file) {
	return true;
    }
    
    public String toString() {
	return "All files (*.*)";
    }
}

