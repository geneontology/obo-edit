package org.obo.dataadapter;

public interface OBOVersionSpec {

	public static final Object HEADER_STANZA = new Object();

	public double getVersion();

	public boolean isAllowed(String stanza);

	public boolean isAllowed(String stanza, String tag);
}
