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
package org.geneontology.refgenome.server;


import java.sql.SQLException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

import org.geneontology.refgenome.client.RefGenomeService;
import org.obd.model.Node;
import org.obd.query.ComparisonQueryTerm;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.OBDSQLShard;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

/**
 * The implemenation of the RPC service which runs on the server.
 */
public class RefGenomeServiceImpl extends RemoteServiceServlet implements
RefGenomeService {

	static String defaultJdbcPath = "jdbc:postgresql://localhost:5432/obd_phenotype_all";

	Shard shard = null;



	public RefGenomeServiceImpl() {
		super();
		setup();
	}

	// TODO: make configurable
	public void setup() {
		if (shard != null)
			return;
		shard = new MultiShard();
		System.err.println("shard="+shard);
		try {		
			OBDSQLShard obd;
			obd = new OBDSQLShard();
			System.err.println("connecting="+shard);
			obd.connect(defaultJdbcPath);
			System.err.println("obd="+obd);
			((MultiShard)shard).addShard(obd);
		} catch (SQLException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.err.println("//shard="+shard);

	}
	public String testCall() {
		System.err.println("foobar");
		setup();
		System.err.println("getNode");
		Node n = shard.getNode("CL:0000148");
		System.err.println("getNode="+n);
		String label = n.getLabel();
		System.err.println("getNode="+label);

		return n.getLabel();
	}

	public String testCall(String id) {
		System.err.println("foobar");
		return "foo";
	}

	public String[] fetchIdsByName(String searchTerm) {
		Collection<Node> nodes;
		try {
			nodes = shard.getNodesBySearch(searchTerm, 
					ComparisonQueryTerm.Operator.STARTS_WITH);
			Collection<String> nids = new LinkedList<String>();
			for (Node n : nodes) {
				System.err.println("n="+n);
				nids.add(n.getId());
			}
			String[] nidArr = 
				(String[]) nids.toArray(new String[0]);
			System.err.println("nids array="+nidArr);
			return nidArr;
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null; // TODO
	}

	public String[] fetchReferenceTaxonIds() {
		return null;
	}

	public String[] fetchEntityIdsInHomologSet(String homologSetId) {
		// TODO Auto-generated method stub
		return null;
	}

	public String[] fetchIdsByName(String searchTerm, String taxonId) {
		// TODO Auto-generated method stub
		return null;
	}

	public Map<String, String> fetchLabelMapsById(String searchTerm) {
		// TODO Auto-generated method stub
		return null;
	}

	public String[] fetchLabelsById(String searchTerm) {
		// TODO Auto-generated method stub
		return null;
	}

	public String[] fetchReferenceTargetIds() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getTaxonIdPrefix() {
		// TODO Auto-generated method stub
		return null;
	}

	public void assignGeneTargetStatus(String geneId)  {
		// TODO Auto-generated method stub
		
	}

	public void retractGeneTargetStatus(String geneId) {
		// TODO Auto-generated method stub
		
	}
	
	
	
}
