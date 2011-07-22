package org.geneontology.gold.io;

import java.io.*;
import java.net.SocketException;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLDataException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.Map.Entry;

import org.forester.phylogeny.Phylogeny;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.io.GafURLFetch;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.geneontology.util.HibPantherID;

import org.forester.io.parsers.nhx.NHXParser;
//import org.forester.phylogeny.Phylogeny;
//import org.forester.phylogeny.PhylogenyNode;

public class PhyloTreeLoader implements Loader {
	GafURLFetch source;
	Collection<PhyloTree> pts = null;
	
	final static String tsvSuffix = ".tsv";
	final static String PANTHER = "PANTHER";
	final static GeneOntologyManager manager = GeneOntologyManager.getInstance();
	
	static private File tmpdir = null;
	static protected Connection connection;
	static private PreparedStatement selectFamily = null;

	private List<DbOperationsListener> dbOperationListeners;
	
	/**
	 * This variable holds the current file path being loaded into database 
	 */
	private String currentSourceFile;
	
	public PhyloTreeLoader() {
		pts = new HashSet<PhyloTree>();

		dbOperationListeners = new ArrayList<DbOperationsListener>();
		
		if (null == connection) {
			//String host = manager.getGolddbHostName();
			String user = manager.getGolddbUserName();
			String pw   = manager.getGolddbUserPassword();
			String db   = manager.getGolddbName();
			String host = manager.getGolddbHostName();
		
			try {
				connection = DriverManager.getConnection("jdbc:postgresql://" + host +"/" + db, user, pw);
			} catch (SQLException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
	}
	
	public void addDbOperationListener(DbOperationsListener listener){
		dbOperationListeners.add(listener);
	}
	
	public void removeDbOperationListener(DbOperationsListener listener){
		dbOperationListeners.remove(listener);
	}
	
	
	/**
	 * getTrees() needs to be run before this can be used.
	 * @throws  
	 */
	@Override
	public boolean isLoadRequired() {
		for (PhyloTree pt : pts) {
			if (pt.isLoadRequired()) {
				return true;
			}
		}
		return false;
	}
	
	public void getTrees() throws IOException{
		while(source.hasNext()){

			InputStream is = (InputStream)source.next();
			this.currentSourceFile = source.getCurrentGafFile();
			pts.add(new PhyloTree(this.currentSourceFile, is));

			
		}
	}

	public void loadThrow() throws SocketException, IOException, URISyntaxException, SQLException, ClassNotFoundException{

		for(DbOperationsListener listener: dbOperationListeners){
			listener.bulkLoadStart();
		}
		
		source.connect();
		pts = new HashSet<PhyloTree>();
		getTrees();
		if (isLoadRequired()){
			hib(writeTSV(pts));
		}
		
		for(DbOperationsListener listener: dbOperationListeners){
			listener.bulkLoadEnd();
		}
		
	}
	
	/**
	 * Call's loadThrow() but dies if there is an error.
	 */
	@Override
	public void load() {
		try {
			loadThrow();
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}

	@Override
	public void setSource(String src) {
		source = new GafURLFetch(src);
	}
	
	public void setSource(File src){
		setSource(src.toURI().toString());
	}
	
	/**
	 * Writes files loaded to TSV files, loadFromFile() needs to be run first.
	 * @return A list of tables to pass to hib()
	 */
	public Collection<File> writeTSV(Collection<PhyloTree> pts){
		
		if (tmpdir == null) {
			File shm = new File("/dev/shm");
			if (shm.isDirectory()) {
				tmpdir = shm;
			}
		}
		List<File> tmpfiles = new Vector<File>(4);
		try {
			tmpfiles.add(java.io.File.createTempFile("family.",           tsvSuffix, tmpdir));
			tmpfiles.add(java.io.File.createTempFile("family_member.",    tsvSuffix, tmpdir));
			tmpfiles.add(java.io.File.createTempFile("bioentity.",        tsvSuffix, tmpdir));
			tmpfiles.add(java.io.File.createTempFile("family_data_dump.", tsvSuffix, tmpdir));
			TableDumper familyDumper         = new TableDumper(tmpfiles.get(0));
			TableDumper familyMemberDumper   = new TableDumper(tmpfiles.get(1));
			TableDumper bioentityDumper      = new TableDumper(tmpfiles.get(2));
			TableDumper familyDataDumpDumper = new TableDumper(tmpfiles.get(3));
			
			Set<String> have = new HashSet<String>(); // avoid dups
			for (PhyloTree pt : pts) {
				if (pt.isLoadRequired()) {
					boolean complete = true;
					String ptId = pt.getId();

					familyDataDumpDumper.dumpRow(ptId, PANTHER, pt.getNHX(), "nhx");

					familyDumper.dumpRow(ptId, pt.getName(), null, "2011-07-11"); //pt.creationDate());
					for (Bioentity be : pt.getBioentities()) {

						String beId = be.getId();
						
						
						familyMemberDumper.dumpRow(ptId, beId);
						if (be.getGafDocument().contentEquals(PANTHER)) {
							if (have.contains(beId)) {
								//System.err.println("dup: " + beId);
							} else {
								bioentityDumper.dumpRow(beId, be.getSymbol(), be.getFullName(), be.getTypeCls(), be.getNcbiTaxonId(), be.getDb(), PANTHER);
								have.add(beId);
								complete = false;
							}
						}
					}
					if (complete) {
						System.err.println(ptId + " complete!");
					}
				}
			}
			
			// if you write another TSV file add a line below;
			familyDumper.close();
			familyMemberDumper.close();
			bioentityDumper.close();
			familyDataDumpDumper.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		
		for (File tmpfile : tmpfiles) {
			tmpfile.deleteOnExit();
		}
		
		return tmpfiles;
	}
	
	public void hib(Collection<File> tmpfiles) throws SQLException, ClassNotFoundException, IOException {
		TsvFileLoader tsvLoader = new TsvFileLoader(manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
				manager.getGolddbName());
		tsvLoader.loadTables(tmpfiles);
	}
	
	
	public static void main(String[] arg) throws SocketException, IOException, URISyntaxException, SQLException, ClassNotFoundException{
		PhyloTreeLoader ptl = new PhyloTreeLoader();
		ptl.setSource(arg[0]);
		ptl.loadThrow();
	}

	class PhyloTree{
		private String id;
		private Phylogeny tree;
		private Map<String,HibPantherID> leaves;
		private Boolean loadRequired = null;
		
		private PhyloTree(String id, BufferedReader br) throws IOException{
			this.id = id;
			leaves = new HashMap<String,HibPantherID>();
			parse(br);
		}
		
		private PhyloTree(String id, Reader r) throws IOException{
			this(id, new BufferedReader(r));
		}
		
		private PhyloTree(String id, InputStream is) throws IOException{
			this(id, new InputStreamReader(is));
		}
				
		private void parse(BufferedReader br) throws IOException{
			// I tried reusing a NHXParser but got some weird results
			NHXParser parser = new NHXParser();

			// get the title and remove the prefix "[title:" and "]"
			String line = br.readLine().substring(7).replaceFirst(".$", "");

			parser.setSource(br.readLine());
			Phylogeny oneTree[] = parser.parse();
			tree = oneTree[0];
			tree.setName(line);

			while ((line = br.readLine()) != null) {
				String[] kv = line.replaceFirst(".$", "").split(":", 2);
				leaves.put(kv[0], new HibPantherID(kv[1].replace('=', ':')));
			}
			br.close();
		}
		
		public boolean isLoadRequired(){
			if (null != loadRequired) {
				return loadRequired;
			}

			try{
				if (null == selectFamily) {
					selectFamily = connection.prepareStatement("SELECT id FROM FAMILY WHERE id=?");
				}
				selectFamily.setString(1, getId());
				ResultSet rs = selectFamily.executeQuery();
				if (rs.next()) {
					//System.err.println(rs.getString(1));
					loadRequired = false;
					if (rs.next()) {
						throw new SQLDataException("Expecting <=1 row, got more then 1.");
					}
				} else {
					loadRequired = true;
				}
			} catch (SQLException e) {
				e.printStackTrace();
				System.exit(1);
			}	
			return loadRequired;
		}
		
		public String getNHX(){
			Phylogeny out = tree.copy();
			for (Entry<String, HibPantherID> me : leaves.entrySet()) {
				out.getNode(me.getKey()).setName(me.getValue().toString());
			}
			return out.toNewHampshireX();
		}
		
		public Collection<Bioentity> getBioentities() {
			Collection<Bioentity> out = new HashSet<Bioentity>();
			for (HibPantherID hpi : leaves.values()) {
				Bioentity got = hpi.bioentity(PANTHER);
				/*
				if (got.getGafDocument().contentEquals(PANTHER)) {
					this.addStat(hpi.getSpeciesCode(), Status.MISSING);
				} else {
					this.addStat(hpi.getSpeciesCode(), Status.FOUND);
				}
				*/
				out.add(got);
			}
			return out;
		}
		

		
		
		/**
		 * @return What goes into the family.id column.
		 */
		public String getId() {
			return PANTHER + ':' + id.substring(0, id.indexOf('.'));
		}
		
		public String getName() {
			return tree.getName();
		}
		
	}
}
