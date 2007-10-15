package org.obo.identifier;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.Link;

public class DefaultLinkIDWarning implements LinkIDWarning {

	protected Link link;
	protected IDWarning parentWarning;
	protected IDWarning typeWarning;

	public DefaultLinkIDWarning(Link link, IDWarning parentWarning,
			IDWarning typeWarning) {
		this.link = link;
		this.parentWarning = parentWarning;
		this.typeWarning = typeWarning;
	}

	public Link getLink() {
		return link;
	}

	public IDWarning getParentWarning() {
		return parentWarning;
	}

	public IDWarning getTypeWarning() {
		return typeWarning;
	}

	public Collection<LinkIDResolution> getResolutions() {
		Collection<LinkIDResolution> resolutions = new LinkedList<LinkIDResolution>();
		if (parentWarning != null) {
			for (IDResolution res : parentWarning.getResolutions()) {
				resolutions.add(new DefaultLinkIDResolution(link, res, null));
			}
		}
		if (typeWarning != null) {
			for (IDResolution res : typeWarning.getResolutions()) {
				resolutions.add(new DefaultLinkIDResolution(link, null, res));
			}
		}
		return resolutions;
	}

	@Override
	public String toString() {
		return link + ", parentWarning=(" + parentWarning + "), typeWarning=("
				+ typeWarning + ")";
	}

}
