package org.bbop.client.Listener;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;
import org.bbop.client.model.DateDTO;

// implemented by: RefGenomeServiceClientImpl
public interface RefGenomeViewListenerI {

	public RefGenomeServiceAsync getRefGenomeService();
	
	public void doLogin(String user, String pass);
	public void fetchTargetIds();
	public void fetchByName(String name);
	public void fetchByNameAndTaxon(String name, String taxonId);
	public void fetchTargetNodesByName(String name);
	public void fetchTaxonNodes();
	public void fetchTargets();
	public void uploadFile(String userId, String filePath, String fileType);
	public void assignEntityTargetStatus(String userId, String id, DateDTO date);
	public void retractEntityTargetStatus(String userId, String id);
	public void cancelFetch();
	
}
