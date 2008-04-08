package org.bbop.server;



import java.sql.SQLException;

import org.bbop.client.RefGenomeService;
import org.bbop.client.model.DateDTO;
import org.bbop.client.model.NodeDTO;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.MutableOBOSessionShard;
import org.obd.query.impl.OBDSQLShard;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

public class RefGenomeServerImpl extends RemoteServiceServlet implements RefGenomeService {
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

	static String userName = "sjcarbon"; //"remote_user";
    static String password = ""; //"glurp";
	static String defaultJdbcPath = "jdbc:postgresql://spitz.lbl.gov:5432/obd_homologene";

	private String currentUserId;
	private DateDTO currentDate;


	private Shard shard = null;
	private Shard speciesInfoShard = null;

	public RefGenomeServerImpl() {
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
			obd.connect(defaultJdbcPath, userName, password);
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

		// Use this in case of mystery errors.
		//		// Optional: Crank up the logging.
		//		Handler[] handlers = Logger.getLogger( "" ).getHandlers();
		//		for ( int index = 0; index < handlers.length; index++ ) {
		//		handlers[index].setLevel( Level.FINE );
		//		}
		//		Logger l1 = Logger.getLogger("org.bbop.rdbms");
		//		Logger l2 = Logger.getLogger("org.obd");
		//		l1.setLevel(Level.FINEST);
		//		l2.setLevel(Level.FINEST);
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	public Boolean checkUserPassword(String userId, String password) {
		// TODO Auto-generated method stub
		boolean logstatus = true;
		return new Boolean(logstatus);
		
	}
	public String[] fetchReferenceTargetIds() {
		return new String[6];
	}
	public NodeDTO[] fetchIdsByName(String searchTerm) {
		// TODO Auto-generated method stub
		return null;
	}
	
	
}
