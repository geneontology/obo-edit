package org.obo.identifier;

public interface IDResolution {

	public String getOriginalID();
	public String getReplacementID();
	public boolean requiresUserIntervention();
}
