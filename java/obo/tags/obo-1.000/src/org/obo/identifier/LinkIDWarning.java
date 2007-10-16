package org.obo.identifier;

import java.util.Collection;

import org.obo.datamodel.Link;

public interface LinkIDWarning {

	public Link getLink();
	public IDWarning getParentWarning();
	public IDWarning getTypeWarning();
	public Collection<LinkIDResolution> getResolutions();
	
}
