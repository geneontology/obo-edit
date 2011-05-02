package org.geneontology.gold.io;

import java.sql.SQLException;
import java.util.*;
import java.io.*;
import java.util.zip.GZIPInputStream;
//import java.text.SimpleDateFormat;

import org.apache.commons.io.filefilter.SuffixFileFilter;

import org.forester.io.parsers.nhx.NHXParser;
import org.forester.phylogeny.Phylogeny;

import org.geneontology.util.*;
import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;


public class PhyloTreeLoader implements Loader {
	private Collection<PhyloTree> sources;
	
	final static FileFilter tff = new SuffixFileFilter(new String[] { ".tree.gz", ".tree" });
	final static String tsvSuffix = ".tsv";
	final static File tmpdir = null; //new File("/dev/shm");
	final static GeneOntologyManager manager = GeneOntologyManager.getInstance();
	
	PhyloTreeLoader() {
		sources = new HashSet<PhyloTree>();
	}
	
	@Override
	public boolean isLoadRequired() {
		// TODO loop through sources and check dates
		return true;
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
	 * Load tree files into memory.
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
				String id = pt.getId();
				//System.err.println(id);

				familyDumper.dumpRow(id, pt.getName(), null, pt.creationDate());
				for (Bioentity be : pt.getBioentities()) {
					familyMemberDumper.dumpRow(id, be.getId());
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
	
	public void setSource(File src) {
		sources.clear();
		addSource(src);
	}
	
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
	
	public static void main(String[] args) throws IOException {
		Loader ptl = new PhyloTreeLoader();
		ptl.setSource(args[0]);
		if (ptl.isLoadRequired()) {
			ptl.load();
		}
	}
	
	class PhyloTree {
		private File source;
		private BufferedReader reader;
		private Phylogeny tree;
		private Map<String,HibPantherID> leaves;

		
		PhyloTree(File src) {
			source = src;
			leaves = new HashMap<String,HibPantherID>();
		}

		public void loadFromFile() throws IOException {
			loadFromFile(new NHXParser());
		}
		
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
					// TODO: warn about missing members
				} else {
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
	
	}

}