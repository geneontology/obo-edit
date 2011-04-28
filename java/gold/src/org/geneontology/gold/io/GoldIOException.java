package org.geneontology.gold.io;

/**
 * This exception is to be thrown during any gold db operations, ontology load, gaf loading, solr loading.
 * @author Shahid Manzoor
 *
 */
public class GoldIOException extends Exception {

	public GoldIOException() {
	}

	public GoldIOException(String message) {
		super(message);
	}

	public GoldIOException(Throwable cause) {
		super(cause);
	}

	public GoldIOException(String message, Throwable cause) {
		super(message, cause);
	}

}
