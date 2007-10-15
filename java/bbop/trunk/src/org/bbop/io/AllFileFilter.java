package org.bbop.io;

import java.io.*;

public class AllFileFilter implements FilenameFilter {

    public boolean accept(File dir, String file) {
	return true;
    }
    
    public String toString() {
	return "All files (*.*)";
    }
}

