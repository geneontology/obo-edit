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
package org.geneontology.refgenome.client;

import java.util.Map;

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



	/**
	 * query ALL nodes by a searchTerm, return matching node IDs
	 * 
	 * returned nodes can represent ontology classes, genes, homologsets etc
	 * @param searchTerm
	 * @return 
	 */
	public String[] fetchIdsByName(String searchTerm);
	
	/**
	 * limits search results by taxon
	 * @param searchTerm       -- eg SOX9
	 * @param taxonId          -- eg NCBITax:7227
	 * @return
	 */
	public String[] fetchIdsByName(String searchTerm, String taxonId);

	public String[] fetchLabelsById(String id);

	//public Map<String,String> fetchLabelMapsById(String searchTerm);
	 /**
	   * @gwt.typeArgs <String,String>
	   */
	public Map fetchLabelMapsById(String searchTerm);

	
	/**
	 * all taxon IDs that are part of some reference set.
	 * the API is extensible to multiple reference sets - this call
	 * will always return the superset 
	 * 
	 * @return IDs for taxon nodes, typically NCBITaxon
	 */
	public String[] fetchReferenceTaxonIds();
	
	public String getTaxonIdPrefix();
	
	/**
	 * @return all target gene Ids
	 */
	public String[] fetchReferenceTargetIds();
	
	public String[] fetchEntityIdsInHomologSet(String homologSetId);
	
	// ===================
	// admin role methods
	// ===================

	public void assignGeneTargetStatus(String geneId);
	public void retractGeneTargetStatus(String geneId);
	
	
	
	
	
	


}
