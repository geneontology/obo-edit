package org.obo.identifier;

import org.obo.datamodel.Link;

public class DefaultLinkIDResolution implements LinkIDResolution {

	protected Link link;
	protected IDResolution parent;
	protected IDResolution type;

	public DefaultLinkIDResolution(Link link, IDResolution parent,
			IDResolution child) {
		super();
		this.link = link;
		this.parent = parent;
		this.type = child;
	}
	
	public static LinkIDResolution getIgnoreResolution(Link link) {
		return new DefaultLinkIDResolution(link, null, null);
	}

	public Link getLink() {
		return link;
	}

	public IDResolution getParentResolution() {
		return parent;
	}

	public IDResolution getTypeResolution() {
		return type;
	}

	public boolean requiresUserIntervention() {
		return (type != null && type.requiresUserIntervention())
				|| (parent != null && parent.requiresUserIntervention());
	}

	public String toString() {
		return "Replace " + link + " "
				+ (parent == null ? "" : "parent=(" + parent.toString() + ")")
				+ " " + (type == null ? "" : "type=(" + type.toString() + ")");
	}
}
