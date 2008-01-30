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
 * The implemenation of the RPC service which runs on the server.

 * @author cjm
 *
 */
public interface RefGenomeService extends RemoteService {

	/**
	 * 
	 * query ALL nodes by a searchTerm, return matching node IDs
	 */
	public String[] fetchIdsByName(String searchTerm);
	
	public String[] fetchIdsByName(String searchTerm, String taxonId);

	public String[] fetchLabelsById(String searchTerm);

	//public Map<String,String> fetchLabelMapsById(String searchTerm);
	public Map fetchLabelMapsById(String searchTerm);

	
	/**
	 * all taxon IDs that are part of some reference set.
	 * the API is extensible to multiple reference sets - this call
	 * will always return the superset 
	 * 
	 * @return IDs for taxon nodes, typically NCBITax
	 */
	public String[] fetchReferenceTaxonIds();
	
	public String getTaxonIdPrefix();
	
	public String[] fetchReferenceTargetIds();
	
	public String[] fetchEntityIdsInHomologSet(String homologSetId);
	
	


}
