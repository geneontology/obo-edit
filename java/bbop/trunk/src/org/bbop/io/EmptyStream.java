package org.bbop.io;

import java.io.*;

/**
 * An output stream that does nothing. Useful for silencing classes that
 * want to write all kinds of junk to a stream
 */
import org.apache.log4j.*;

public class EmptyStream extends OutputStream {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EmptyStream.class);
    public void write(int b) {
	// do nothing
    }
}
