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

//add methods here from RefGenomeServiceSleep once they gets implemented



//import org.bbop.client.model.DateDTO;
//import org.bbop.client.model.NodeDTO;
//import org.bbop.client.model.StatementDTO;

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
	// admin 
	// ===================
	
	public Boolean checkUserPassword(String userId, String password); // encrypt?
		
	public String[] fetchReferenceTargetIds();

}
