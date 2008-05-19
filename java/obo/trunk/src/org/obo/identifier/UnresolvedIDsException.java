package org.obo.identifier;

import java.util.Collection;

import org.apache.log4j.*;

public class UnresolvedIDsException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UnresolvedIDsException.class);

	protected Collection<LinkIDWarning> warnings;

	public UnresolvedIDsException(Collection<LinkIDWarning> warnings) {
		this.warnings = warnings;
	}

	public Collection<LinkIDWarning> getWarnings() {
		return warnings;
	}
}
