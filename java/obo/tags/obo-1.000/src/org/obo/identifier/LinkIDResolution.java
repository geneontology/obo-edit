package org.obo.identifier;

import org.obo.datamodel.Link;

public interface LinkIDResolution {
	public Link getLink();
	public IDResolution getParentResolution();
	public IDResolution getTypeResolution();
}
