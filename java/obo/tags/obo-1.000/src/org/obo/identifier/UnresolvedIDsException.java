package org.obo.identifier;

import java.util.Collection;

public class UnresolvedIDsException extends Exception {

	protected Collection<LinkIDWarning> warnings;

	public UnresolvedIDsException(Collection<LinkIDWarning> warnings) {
		this.warnings = warnings;
	}

	public Collection<LinkIDWarning> getWarnings() {
		return warnings;
	}
}
