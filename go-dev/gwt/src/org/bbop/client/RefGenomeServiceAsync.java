/*
 * Copyright 2006 Google Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package org.bbop.client;

import org.bbop.client.model.DateDTO;
import com.google.gwt.user.client.rpc.AsyncCallback;

/**
 * The implemenation of the RPC service which runs on the server.
 */
public interface RefGenomeServiceAsync  {
	
	public void fetchNodesByName(String searchTerm, AsyncCallback callback);
	
	public void fetchIdsByNameAndTaxon(String searchTerm, String taxonId, AsyncCallback callback);

	public void fetchReferenceTaxonIds( AsyncCallback callback);
	public void fetchReferenceTaxonNodes( AsyncCallback callback);

	public void fetchEntityIdsInHomologSet(String homologSetId, AsyncCallback callback);
	
	public void fetchLabelMapsById(String searchTerm, AsyncCallback callback);
	
	public void fetchLabelsById(String searchTerm, AsyncCallback callback);

	public void fetchReferenceTargetIds( AsyncCallback callback);
	
	public void getTaxonIdPrefix( AsyncCallback callback);
	
	public void addUser(String userId, String fullName, String password, AsyncCallback callback);

	public void assignEntityComprehensivelyAnnotatedStatus(String userId, String geneId, DateDTO date, AsyncCallback callback);

	public void assignEntityStatusCode(String userId, String statusId, String geneId, DateDTO date, AsyncCallback callback);

	public void assignEntityTargetStatus(String userId, String geneId, DateDTO date, AsyncCallback callback);

	public void assignHomologyLinkStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment, AsyncCallback callback);

	public void assignNegativeHomologyLinkStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment, AsyncCallback callback);

	public void attachCommentToEntity(String entityId, String comment, String curatorId, AsyncCallback callback);

	public void checkUserPassword(String userId, String password, AsyncCallback callback);

	public void fetchHomologousEntityIds(String entityId, AsyncCallback callback);

	public void fetchHomologyLinkStatementsByEntityId(String entityId, AsyncCallback callback);
	
	public void fetchNodeById(String id, AsyncCallback callback);
	
	public void getAllUsers(AsyncCallback callback);
	
	public void uploadFile(String userId, String filePath, String fileType, AsyncCallback callback);


	public void getHomologyMethodTypeNodes(AsyncCallback callback);

	public void getStatusCodeNodes(AsyncCallback callback);

	public void retractEntityComprehensivelyAnnotatedStatus(String userId, String geneId, AsyncCallback callback);

	public void retractEntityStatusCode(String userId, String statusId, String geneId, AsyncCallback callback);

	public void retractEntityTargetStatus(String userId, String geneId, AsyncCallback callback);
	
	public void isSetComprehensivelyAnnotated(String entityId, AsyncCallback callback);

}
