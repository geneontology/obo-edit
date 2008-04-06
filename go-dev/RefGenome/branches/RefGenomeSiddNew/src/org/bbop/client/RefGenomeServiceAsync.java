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




import org.bbop.client.model.NodeDTO;

import com.google.gwt.user.client.rpc.AsyncCallback;

/**
 * The implemenation of the RPC service which runs on the server.
 */
public interface RefGenomeServiceAsync  {
	
	
	
	public void checkUserPassword(String userId, String password, AsyncCallback callback); 

	public void fetchReferenceTargetIds( AsyncCallback callback);
	
	public void fetchIdsByName(String searchTerm, AsyncCallback callback);
}
