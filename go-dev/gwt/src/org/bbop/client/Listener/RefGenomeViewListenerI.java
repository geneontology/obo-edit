package org.bbop.client.Listener;

import org.bbop.client.model.DateDTO;

public interface RefGenomeViewListenerI {

	public void doLogin(String user, String pass);
	public void fetchTargetIds();
	public void fetchByName(String name);
	public void fetchByNameAndTaxon(String name, String taxonId);
	public void fetchTargetNodesByName(String name);
	public void fetchTaxonNodes();
	public void uploadFile(String userId, String filePath, String fileType);
	public void fetchTargets();
	public void assignEntityTargetStatus(String userId, String id, DateDTO date);
	public void retractEntityTargetStatus(String userId, String id);
	public void cancelFetch();
	
}
