package org.bbop.client;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface RefGenomeAsyncSleep {
	public void fetchIdsByName(String searchTerm, AsyncCallback callback);

	public void fetchIdsByName(String searchTerm, String taxonId, AsyncCallback callback);

	public void fetchLabelsById(String searchTerm, AsyncCallback callback);

	public void fetchLabelMapsById(String searchTerm, AsyncCallback callback);

	public void fetchReferenceTaxonIds( AsyncCallback callback);
	
	public void getTaxonIdPrefix( AsyncCallback callback);
	
	public void fetchReferenceTargetIds( AsyncCallback callback);
	
	public void fetchEntityIdsInHomologSet(String homologSetId, AsyncCallback callback);

	public void assignGeneTargetStatus(String geneId, AsyncCallback callback) ;

	public void retractGeneTargetStatus(String geneId, AsyncCallback callback) ;
	
	public void addUser(String userId, String fullName, String password, AsyncCallback callback); 
	
	public void checkUserPassword(String userId, String password, AsyncCallback callback); 


}
