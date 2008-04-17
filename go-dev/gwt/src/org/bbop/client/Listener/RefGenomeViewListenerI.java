package org.bbop.client.Listener;

public interface RefGenomeViewListenerI {

	public void doLogin(String user, String pass);
	public void fetchTargetIds();
	public void fetchByName(String name);
	public void fetchTaxonNodes();
}
