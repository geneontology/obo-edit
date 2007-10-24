package org.obo.identifier;

import org.obo.datamodel.Link;

public class DefaultLinkIDResolution implements LinkIDResolution {

	protected Link link;
	protected IDResolution parent;
	protected IDResolution child;

	public DefaultLinkIDResolution(Link link, IDResolution parent,
			IDResolution child) {
		super();
		this.link = link;
		this.parent = parent;
		this.child = child;
	}

	public Link getLink() {
		return link;
	}

	public IDResolution getParentResolution() {
		return parent;
	}

	public IDResolution getTypeResolution() {
		return child;
	}

	public String toString() {
		return "Replace " + link + " "
				+ (parent == null ? "" : "parent=(" + parent.toString()+")") + " "
				+ (child == null ? "" : "child=(" + child.toString()+")");
	}
}
