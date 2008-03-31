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
package org.bbop.server;


import java.sql.SQLException;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import org.bbop.client.RefGenomeService;
import org.bbop.client.model.DateDTO;
import org.bbop.client.model.NodeDTO;
import org.bbop.client.model.StatementDTO;
import org.bbop.dataadapter.DataAdapterException;
import org.obd.model.Graph;
import org.obd.model.LinkStatement;
import org.obd.model.Node;
import org.obd.model.Statement;
import org.obd.query.ComparisonQueryTerm;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.MutableOBOSessionShard;
import org.obd.query.impl.OBDSQLShard;
import org.obd.query.impl.OBOSessionShard;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

/**
 * The implemenation of the RPC service which runs on the server.
 */
public class RefGenomeServiceImpl extends RemoteServiceServlet implements RefGenomeService {

	// Vocabulary
	private String HAS_STATUS = "RefG:has_status";
	private String HAS_EX_STATUS = "RefG:has_ex_status";
	private String STATUS_TARGET = "RefG:Target";
	private String HOMOLOGOUS_TO = "OBO_REL:homologous_to";
	private String HAS_PROVENANCE = "oban:has_data_source";
	private String HAS_EVIDENCE = "oban:has_evidence";
	private String HAS_COMMENT = "dc:comment";
	private String STATUS_COMPREHENSIVELY_ANNOTATED = "RefG:comprehensively_annotated";
	private String refGenomeSpeciesPath = "ftp://ftp.geneontology.org/pub/go/doc/reference-genome-species.obo";
	private String ON_DATE = "dc:date";

	static String defaultJdbcPath = "jdbc:postgresql://spitz.lbl.gov:5432/obd_homologene";

	private String currentUserId;
	private DateDTO currentDate;


	private Shard shard = null;
	private Shard speciesInfoShard = null;

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
		speciesInfoShard = new MutableOBOSessionShard();
		try {
			((MutableOBOSessionShard)speciesInfoShard).loadFile(refGenomeSpeciesPath);
		} catch (DataAdapterException e) {
			// TODO : backup plan?
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

	// NOTE: This doesn't seem to do anything.
	public String[] fetchIdsByName(String searchTerm) {
		Collection<Node> nodes;
		try {
			nodes = shard.getNodesBySearch(searchTerm, 
					ComparisonQueryTerm.Operator.STARTS_WITH); // TODO
			Collection<String> nids = new LinkedList<String>();
			System.err.println("nodes size = "+ nodes.size() + "");
			for (Node n : nodes) {
				System.err.println("n="+n);
				nids.add(n.getId());
			}
			String[] nidArr = 
				(String[]) nids.toArray(new String[0]);
			System.err.println("nids array("+ nidArr.length + ")="+nidArr);
			return nidArr;
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null; // TODO
	}

	public String[] fetchIdsByNameAndTaxon(String searchTerm, String taxonId) {
		// TODO Auto-generated method stub
		return null;
	}
	
	// NOTE: This works fine.
	public String[] fetchReferenceTaxonIds() {
		return nodesToIds(speciesInfoShard.getNodes());
	}

	private String[] nodesToIds(Collection<Node> nodes) {
		String[] nids = new String[nodes.size()];
		Iterator<Node> it = nodes.iterator();
		for (int i=0; it.hasNext(); i++) {
			nids[i] = it.next().getId();
		}
		return nids;
	}

	public String[] fetchEntityIdsInHomologSet(String homologSetId) {
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
		String[] sa =  new String[0];
		return sa;
	}

	public String getTaxonIdPrefix() {
		// TODO Auto-generated method stub
		return null;
	}

	public void addUser(String userId, String fullName, String password) {
		// TODO Auto-generated method stub

	}


	@Deprecated
	public void retractGeneTargetStatus(String userId, String geneId) {
		retractStatement(geneId,HAS_STATUS,STATUS_TARGET,userId,currentDate);	
	}


	public void assignEntityComprehensivelyAnnotatedStatus(String userId, String geneId, DateDTO date) {
		addStatement(geneId,HAS_STATUS,STATUS_COMPREHENSIVELY_ANNOTATED,userId,date);	
	}



	public void assignEntityStatusCode(String userId, String statusId, String geneId, DateDTO date) {
		addStatement(geneId,HAS_STATUS,statusId,userId,date);	
	}

	public void assignEntityTargetStatus(String userId, String geneId, DateDTO date) {
		addStatement(geneId,HAS_STATUS,STATUS_TARGET,userId,currentDate);	
	}

	private Statement makeHomologyStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment) {
		LinkStatement s = new LinkStatement(e1id,HOMOLOGOUS_TO,e2id,userId);
		s.addSubLiteralStatement(ON_DATE, currentDate.toString());
		s.addSubLiteralStatement(HAS_PROVENANCE, provenanceId);
		for (String methodId : methodIds)
			s.addSubLiteralStatement(HAS_EVIDENCE, methodId);
		if (comment != null)
			s.addSubLiteralStatement(HAS_COMMENT, comment);
		return s;
	}
	
	public void assignHomologyLinkStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment) {
		Graph g = new Graph();
		Statement s = makeHomologyStatement(userId,e1id,e2id,methodIds,provenanceId,comment);
		g.addStatement(s);
		shard.putGraph(g);
	}

	public void assignNegativeHomologyLinkStatement(String userId, String e1id, String e2id, String[] methodIds, String provenanceId, String comment) {
		Graph g = new Graph();
		Statement s = makeHomologyStatement(userId,e1id,e2id,methodIds,provenanceId,comment);
		s.setNegated(true);
		g.addStatement(s);
		shard.putGraph(g);

	}

	public void attachCommentToEntity(String entityId, String comment, String curatorId) {
		// TODO Auto-generated method stub

	}

	public void checkUserPassword(String userId, String password) {
		// TODO Auto-generated method stub

	}

	public String[] fetchHomologousEntityIds(String entityId) {
		// TODO Auto-generated method stub
		return null;
	}

	public StatementDTO[] fetchHomologyLinkStatementsByEntityId(String entityId) {
		// TODO Auto-generated method stub
		return null;
	}

	public NodeDTO fetchNodeById(String id) {
		// TODO Auto-generated method stub
		return null;
	}

	public NodeDTO[] getAllUsers() {
		// TODO Auto-generated method stub
		return null;
	}

	public NodeDTO[] getHomologyMethodTypeNodes() {
		// TODO Auto-generated method stub
		return null;
	}

	public NodeDTO[] getStatusCodeNodes() {
		// TODO Auto-generated method stub
		return null;
	}

	public void retractEntityComprehensivelyAnnotatedStatus(String userId, String geneId) {
		// TODO Auto-generated method stub

	}

	public void retractEntityStatusCode(String userId, String statusId, String geneId) {
		// TODO Auto-generated method stub

	}

	public void retractEntityTargetStatus(String userId, String geneId) {
		// TODO Auto-generated method stub

	}

	private void addStatement(String su, String rel, String ob, String userId, DateDTO date) {
		LinkStatement s = new LinkStatement(su,rel,ob,userId);
		s.addSubLiteralStatement(ON_DATE, date.toString());
		Graph g = new Graph();
		g.addStatement(s);
		shard.putGraph(g);
	}

	private void retractStatement(String su, String rel, String ob, String userId, DateDTO date) {
		Collection<Statement> stmts = shard.getStatements(su, rel, ob, null, null, null);
		for (Statement s : stmts) {
			s.setRelationId(HAS_EX_STATUS);
		}
		Graph g = new Graph(stmts);
		shard.putGraph(g);
	}


}
