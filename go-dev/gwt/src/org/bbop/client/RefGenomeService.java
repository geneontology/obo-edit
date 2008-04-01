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

import java.util.Map;

import org.bbop.client.model.NodeDTO;

import com.google.gwt.user.client.rpc.RemoteService;

/**
 * A Facade for accessing a RefG repository
 * 
 * All IDs are assumed to uniquely identify the object of interest, and
 * are of the form DB : LocalID
 * 
 * 
 * @author cjm
 *
 */
public interface RefGenomeService extends RemoteService {


	// ===================
	// user operations 
	// ===================
	
	/**
	 * query ALL nodes by a searchTerm, return matching nodes
	 * 
	 * returned nodes can represent ontology classes, genes, homologsets etc
	 * @param searchTerm
	 * @return 
	 */
	public NodeDTO[] fetchIdsByName(String searchTerm);
	//public String[] fetchIdsByName(String searchTerm);
	//public String[] fetchIdsByName(String searchTerm, String operator);

	
	/**
	 * limits search results by taxon
	 * @param searchTerm       -- eg SOX9
	 * @param taxonId          -- eg NCBITax:7227
	 * @return
	 */
	public String[] fetchIdsByNameAndTaxon(String searchTerm, String taxonId);

	/**
	 * Given an Id, return all labels, names, synonyms etc by which this Id is
	 * known
	 * 
	 * @param id
	 * @return
	 */
	public String[] fetchLabelsById(String id);

	//public NodeDTO fetchNodeById(String id);

	//public Map<String,String> fetchLabelMapsById(String searchTerm);
	 /**
	   * @gwt.typeArgs <java.lang.String,java.lang.String>
	   */
	public Map fetchLabelMapsById(String searchTerm);

	
	/**
	 * all taxon IDs that are part of some reference set.
	 * the API is extensible to multiple reference sets - this call
	 * will always return the superset. In the case of the RefG
	 * project, there should always be 12 IDs returned
	 * 
	 * @return IDs for taxon nodes, typically NCBITaxon
	 */
	public String[] fetchReferenceTaxonIds();
	
	/**
	 * typical NCBITaxon
	 * @return IDSpace of the taxononymy database
	 */
	public String getTaxonIdPrefix();
	
	/**
	 * @return all target gene Ids
	 */
	public String[] fetchReferenceTargetIds();
	
	/**
	 * Given an ID for the homology set, return the IDs of all members
	 * @param homologSetId
	 * @return
	 */
	//public String[] fetchEntityIdsInHomologSet(String homologSetId);
	
	/**
	 * given a gene (or similar entity) return all homologs
	 * @param entityId  -- e.g. a gene or gene product Id
	 * @return
	 */
	//public String[] fetchHomologousEntityIds(String entityId);
	
	/**
	 * given an entity Id (e.g. a MOD gene ID), return all edges in
	 * the homology graph
	 * 
	 * @param entityId
	 * @return
	 */
	//public StatementDTO[] fetchHomologyLinkStatementsByEntityId(String entityId);
	
	// ===================
	// admin 
	// ===================
	//public void addUser(String userId, String fullName, String password); // encrypt?
	//public void checkUserPassword(String userId, String password); // encrypt?
	//public NodeDTO[] getAllUsers();

	// ===================
	// big-cheese role methods
	// ===================

	//public NodeDTO[] getStatusCodeNodes();
	
	// TODO: do each of these require a userId? Is this stateful?
	
	// generic status operations: actual status codes are extensible
	//public void assignEntityStatusCode(String userId, String statusId, String geneId, DateDTO date); // datatype?
	//public void retractEntityStatusCode(String userId, String statusId, String geneId); // datatype?

	// these two methods are convenience wrappers for the above
	//public void assignEntityTargetStatus(String userId, String geneId, DateDTO date); // datatype?
	//public void retractEntityTargetStatus(String userId, String geneId);
	
	// these two methods are convenience wrappers for the above
	//public void assignEntityComprehensivelyAnnotatedStatus(String userId, String geneId, DateDTO date);
	//public void retractEntityComprehensivelyAnnotatedStatus(String userId, String geneId);

	//public void attachCommentToEntity(String entityId, String comment, String curatorId);
	
	/**
	 * e.g. PPOD, Homologene, ...
	 * @return list of node objects, with IDs, labels and descriptions
	 */
	//public NodeDTO[] getHomologyMethodTypeNodes();
	//public void assignHomologyLinkStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment);
	//public void assignNegativeHomologyLinkStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment);
	


}
