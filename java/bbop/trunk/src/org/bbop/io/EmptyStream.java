package org.bbop.io;

import java.io.*;

/**
 * An output stream that does nothing. Useful for silencing classes that
 * want to write all kinds of junk to a stream
 */
public class EmptyStream extends OutputStream {
    public void write(int b) {
	// do nothing
    }
}
