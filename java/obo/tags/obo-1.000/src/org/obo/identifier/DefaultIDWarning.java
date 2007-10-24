package org.obo.identifier;

import java.util.Collection;

public class DefaultIDWarning implements IDWarning {
	
	protected String id;
	protected Collection<IDResolution> resolutions;
	protected WarningType type;

	public DefaultIDWarning(String id, Collection<IDResolution> resolutions,
			WarningType type) {
		this.id = id;
		this.resolutions = resolutions;
		this.type = type;
	}

	public String getID() {
		return id;
	}

	public Collection<IDResolution> getResolutions() {
		return resolutions;
	}

	public WarningType getType() {
		return type;
	}

	public String toString() {
		return type+" - "+id+" "+resolutions;
	}
}
