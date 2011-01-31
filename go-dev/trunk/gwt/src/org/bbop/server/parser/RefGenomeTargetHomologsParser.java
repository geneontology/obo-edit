package org.bbop.server.parser;

import java.util.ArrayList;
import java.util.Collection;

import org.bbop.client.RefGenomeService;
import org.bbop.client.model.DateDTO;
import org.bbop.client.model.NodeDTO;
import org.obd.model.LiteralStatement;
import org.obd.model.Node;
import org.obd.model.NodeAlias;
import org.obd.model.Statement;
import org.obd.model.LiteralStatement.Datatype;
import org.obd.parser.TabularInfoParser;
import org.obd.query.Shard;


/**
 * Parses tab-delimited files from NCBI Gene (aka EntrezGene, formerly LocusLink)
 * @author cjm
 *
 */
public class RefGenomeTargetHomologsParser extends TabularInfoParser {

	protected RefGenomeService refgService;
	protected String src = "NCBI:gene";
	protected String userId = "system";
	protected DateDTO date = new DateDTO(2008,1,1); // TODO
	protected Collection<String> unparsedIds = new ArrayList<String>();
	
	public RefGenomeTargetHomologsParser(String path) {
		super(path);
	}
	
	public static String getDefaultURL() {
		return "ftp://ftp.informatics.jax.org/pub/curatorwork/GODB/refg_id_list.txt";
	}


	public void parseColVals(String[] colVals) {
		if (colVals[0].startsWith("!"))
			return;
		if (colVals[0].startsWith("target_id"))
			return;
		
		String targetId = colVals[0];
		String modId = colVals[2];
		String modSym = colVals.length > 3 ? colVals[3] : null;
		NodeDTO tNode = refgService.fetchNodeById(targetId);
		if (tNode == null) {
			addUnparsedId(targetId);
		}
		NodeDTO mNode = refgService.fetchNodeById(modId);
		if (mNode == null) { // hack to account for MGI etc
			String[] parts = modId.split(":",2);
			String modifiedModId = parts[0]+":"+modId;
			mNode = refgService.fetchNodeById(modifiedModId);
		}
		if (mNode == null) {
			addUnparsedId(modId);
		}
		// TODO - notify if not found
		refgService.assignEntityTargetStatus(userId, targetId, date);
		String[] methodIds = { "refG:unknown" };
		refgService.assignHomologyLinkStatement(userId, modId, targetId, methodIds, null, "imported from google spreadsheets");
		
	}
	
	public void addUnparsedId(String id) {
		unparsedIds.add(id);
	}

	public RefGenomeService getRefgService() {
		return refgService;
	}

	public void setRefgService(RefGenomeService rgs) {
		this.refgService = rgs;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public DateDTO getDate() {
		return date;
	}

	public void setDate(DateDTO date) {
		this.date = date;
	}

	
	
}
