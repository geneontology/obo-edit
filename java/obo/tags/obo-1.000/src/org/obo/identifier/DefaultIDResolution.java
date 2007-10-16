package org.obo.identifier;

public class DefaultIDResolution implements IDResolution {
	
	protected String id;
	protected String newid;
	protected boolean user;
	
	public DefaultIDResolution(String id, String newid, boolean user) {
		this.id = id;
		this.newid = newid;
		this.user = user;
	}

	public String getOriginalID() {
		return id;
	}

	public String getReplacementID() {
		return newid;
	}

	public boolean requiresUserIntervention() {
		return user;
	}
	
	public String toString() {
		return "Replace "+id+" with "+newid+" "+(user ? "" : "auto");
	}

}
