package org.geneontology.gold.io;

import java.util.*;
import java.util.Date;
import java.io.*;
import java.sql.*;
import java.sql.DriverManager;
import java.sql.SQLException;

import java.util.zip.GZIPInputStream;

import org.apache.commons.io.filefilter.SuffixFileFilter;

import org.forester.io.parsers.nhx.NHXParser;
import org.forester.phylogeny.Phylogeny;

import org.geneontology.util.*;
import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;

/**
 * 
 * @author Sven Heinicke
 *
 */
public class PhyloTreeLoader implements Loader {
	private Collection<PhyloTree> sources;
	

	protected enum Status { FOUND, MISSING };
	
	/**
	 * Try to match files that are created by the {@link org.paint.util.PaintScraper} class.
	 */
	final static FileFilter tff = new SuffixFileFilter(new String[] { ".tree.gz", ".tree" });
	
	final static String tsvSuffix = ".tsv";
	final static File tmpdir = null; //new File("/dev/shm");
	final static GeneOntologyManager manager = GeneOntologyManager.getInstance();

	static private PreparedStatement selectFamily = null;
	static protected Map<String,Map<Status,Integer>> count = null;
	static protected Connection connection;
	
	PhyloTreeLoader() {
		sources = new HashSet<PhyloTree>();

		if (null == connection) {
			//String host = manager.getGolddbHostName();
			String user = manager.getGolddbUserName();
			String pw   = manager.getGolddbUserPassword();
			String db   = manager.getGolddbName();
		
			try {
				connection = DriverManager.getConnection("jdbc:postgresql://localhost/" + db, user, pw);
			} catch (SQLException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
	}
		
	@Override
	public boolean isLoadRequired() {
		for (PhyloTree pt : sources) {
			if (pt.isLoadRequired()) {
				return true;
			}
		}

		return false;
	}

	@Override
	public void load() {
		loadFromFile();
		List<File> tmpfiles = writeTSV();
		try {
			hib(tmpfiles);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Load sources into memory, assumes the files are created by the {@link org.paint.util.PaintScraper} class.
	 *
	 * If there is a problem loading the files it prints a stack trace, and removes that files from the source collection.
	 */
	protected void loadFromFile() {
		NHXParser parser = new NHXParser();
		Collection<PhyloTree> removeMe = new HashSet<PhyloTree>();
		
		for (PhyloTree pt : sources) {
			try {
				pt.loadFromFile(parser);
			} catch (IOException e) {
				e.printStackTrace();
				removeMe.add(pt);
			}
		}
		sources.removeAll(removeMe);		
	}

	/**
	 * Writes files loaded to TSV files, loadFromFile() needs to be run first.
	 * @return A list of tables to pass to hib()
	 */
	protected List<File> writeTSV() {
		List<File> tmpfiles = new Vector<File>(2);
		try {
			tmpfiles.add(java.io.File.createTempFile("family.",        tsvSuffix, tmpdir));
			tmpfiles.add(java.io.File.createTempFile("family_member.", tsvSuffix, tmpdir));
			TableDumper familyDumper       = new TableDumper(tmpfiles.get(0));
			TableDumper familyMemberDumper = new TableDumper(tmpfiles.get(1));
			for (PhyloTree pt : sources) {
				if (pt.isLoadRequired()) {
					String id = pt.getId();

					familyDumper.dumpRow(id, pt.getName(), null, pt.creationDate());
					for (Bioentity be : pt.getBioentities()) {
						familyMemberDumper.dumpRow(id, be.getId());
					}
				}
			}
			familyDumper.close();
			familyMemberDumper.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		
		for (File tmpfile : tmpfiles) {
			tmpfile.deleteOnExit();
		}
		return tmpfiles;
	}
	

	
	protected void hib(List<File> tmpfiles) throws SQLException, ClassNotFoundException, IOException {
		TsvFileLoader tsvLoader = new TsvFileLoader(manager.getGolddbUserName(),
				manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
				manager.getGolddbName());
		tsvLoader.loadTables(tmpfiles);
	}
	
	@Override
	public void setSource(String src) {
		setSource(new File(src));
	}
	
	/**
	 * Clears the current set of sources.
	 * @param src Passed as: addSource(src);
	 */
	public void setSource(File src) {
		System.err.println("setSource(" + src + ")");
		sources.clear();
		addSource(src);
	}
	
	/**
	 * @param src Passed as: addSource(new File(src));
	 */
	public void addSource(String src) {
		addSource(new File(src));
	}
	
	public void addSource(File src) {
		if (src.isDirectory()) {
			for (File s : src.listFiles(tff)) {
				sources.add(new PhyloTree(s));
			}		
		} else {
			sources.add(new PhyloTree(src));
		}
	}
	
	/**
	 * 
	 * @param args List of paths that point to files created by the {@link org.paint.util.PaintScraper} class.
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		count = new HashMap<String,Map<Status,Integer>>();
		Loader ptl = new PhyloTreeLoader();
		for (String arg : args) {
			ptl.setSource(arg);
		}
		if (ptl.isLoadRequired()) {
			ptl.load();
		}
		
		for (String key : new TreeSet<String>(count.keySet())) {
			System.out.println(key + '=' + count.get(key));
		}
	}
	
	/**
	 * An individual file created by the {@link org.paint.util.PaintScraper} class.
	 *
	 */
	class PhyloTree {
		private File source;
		private BufferedReader reader;
		private Phylogeny tree;
		private Map<String,HibPantherID> leaves;
		private Boolean loadRequired = null;
		
		/**
		 * 
		 * @param src A File object that should represent a file created by the {@link org.paint.util.PaintScraper} class.
		 */
		PhyloTree(File src) {
			this.source = src;
			leaves = new HashMap<String,HibPantherID>();
		}
		

		
		/**
		 * Currently it only checks if the tree has already been loaded. Nothing else.
		 * @return true if the tree needs to be loaded
		 */
		boolean isLoadRequired(){
			if (null != loadRequired) {
				return loadRequired;
			}

			try {
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
		
		private void addStat(String key, Status status) {
			if (null == count) {
				return;
			}
			
			//System.err.println("addStat(" + key + "," + status + ')');
			
			if (count.containsKey(key)) {
				Map<Status,Integer> value = count.get(key);
				if (value.containsKey(status)) {
					value.put(status, 1 + value.get(status));
				} else {
					value.put(status, 1);
				}
			} else {
				Map<Status,Integer> value = new HashMap<Status,Integer>();
				value.put(status, 1);
				count.put(key, value);
			}
		}
		
		/**
		 * Calls loadFromFile(NHXParser) with a newly created {@link org.forester.io.parsers.nhx.NHXParser} object.
		 * @throws IOException
		 */
		public void loadFromFile() throws IOException {
			loadFromFile(new NHXParser());
		}

		/**
		 * @param parser
		 * @throws IOException
		 */
		public void loadFromFile(NHXParser parser) throws IOException {
			open();

			// get the title and remove the prefix "[title:" and "]"
			String line = reader.readLine().substring(7).replaceFirst(".$", "");

			parser.setSource(reader.readLine());
			Phylogeny oneTree[] = parser.parse();
			tree = oneTree[0];
			tree.setName(line);

			while ((line = reader.readLine()) != null) {
				String[] kv = line.replaceFirst(".$", "").split(":", 2);
				leaves.put(kv[0], new HibPantherID(kv[1].replace('=', ':')));
			}

			close();
		}
		
		/**
		 * 
		 * @return A BufferedReader class of the file.
		 * @throws IOException
		 */
		BufferedReader open() throws IOException
		{
			FileInputStream fis = new FileInputStream(source);
			InputStreamReader isr = null;
			if (gzipped()) {
				GZIPInputStream gis = new GZIPInputStream(fis);
				isr = new InputStreamReader(gis);
			} else {
				isr = new InputStreamReader(fis);
			}
			reader = new BufferedReader(isr);
			return reader;
		}
		
		/**
		 * Closes the reader opened by open().
		 * @throws IOException
		 */
		void close() throws IOException
		{
			reader.close();
		}

		boolean gzipped() {
			return source.getName().endsWith(".gz");
		}
		
		/**
		 * @return What goes into the family.id column.
		 */
		public String getId() {
			String id = source.getName();
			return "PANTHER:" + id.substring(0, id.indexOf('.'));
		}
		
		public String getName() {
			return tree.getName();
		}
		
		public Collection<Bioentity> getBioentities() {
			Collection<Bioentity> out = new HashSet<Bioentity>();
			for (HibPantherID hpi : leaves.values()) {
				Bioentity got = hpi.bioentity();
				if (got == null) {
					this.addStat(hpi.getSpeciesCode(), Status.MISSING);
					if (hpi.isRefG()) { // only warn about missing refg members
						System.err.println(hpi + " matched no bioentities");
					}
				} else {
					this.addStat(hpi.getSpeciesCode(), Status.FOUND);
					out.add(hpi.bioentity());
				}
			}
			return out;
		}
		
		public long lastModified() {
			return source.lastModified();
		}
		
		public String creationDate(){
			return manager.SimpleDateFormat().format(new Date(lastModified()));
		}
		
		public String toString(){
			return source.toString();
		}
	
	}

}