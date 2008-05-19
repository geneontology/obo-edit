package org.obo.identifier;

import java.util.Collection;

import org.apache.log4j.*;

public class DefaultIDWarning implements IDWarning {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultIDWarning.class);
	
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
