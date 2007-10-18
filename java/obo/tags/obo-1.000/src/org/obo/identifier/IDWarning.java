package org.obo.identifier;

import java.util.Collection;

public interface IDWarning {

	public static enum WarningType { SECONDARY_ID, OBSOLETE_ID, DANGLING_ID};

	public String getID();
	public Collection<IDResolution> getResolutions();
	public WarningType getType();
}
