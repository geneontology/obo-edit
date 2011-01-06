package org.geneontology.gold.io;

import java.io.*;
import java.util.zip.GZIPInputStream;

import java.util.Arrays;
import java.util.Collection;
import java.util.TreeSet;

import java.util.Map;
import java.util.TreeMap;

import org.geneontology.util.*;

import org.forester.io.parsers.nhx.NHXParser;
import org.forester.phylogeny.Phylogeny;

/**
 * loads phylogenetic trees into database
 * 
 * From NHXish file. Reads files output from org.paint.util.PaintScraper class.
 *
 */
public class PhyloTreeLoader implements Loader {
	private File file;
	private BufferedReader reader;
	private Phylogeny tree;
	private Map<String,PantherID> leaves;
	
	static NHXParser parser = null;
	
	PhyloTreeLoader() {
		reader = null;
		if (parser == null) {
			parser = new NHXParser();
		}
	}

	/**
	 * 
	 * @param pathname Pathname is used to create a File object, and then sent to the other setSourc().
	 */
	public void setSource(String pathname) {
		setSource(new File(pathname));
	}
	
	/**
	 * Clears data and sets new file.
	 * @param file File object to be loaded
	 */
	public void setSource(File file) {
		this.file = file;
		reader = null;
		tree = null;
		leaves = new TreeMap<String,PantherID>();
	
	}
	
	public boolean isLoadRequired() {
		// TODO: compare file.lastModifiet() to family.creation_date
		
		return true;
	}
	
	public void load() {
		try {
			loadFromFile();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		// writeToGold();
	}
	
	/**
	 * 
	 * @param file - File object painting to file produced by org.paint.util.PaintScraper class. Can be gzipped.
	 * @throws IOException If it has problems accessing the file.
	 */
	public void loadFromFile() throws IOException {
		open();
			
		// get the title and remove the prefix "[title:" and "]"
		String line = reader.readLine().substring(7).replaceFirst(".$", "");

		parser.setSource(reader.readLine());
		Phylogeny oneTree[] = parser.parse();
		tree = oneTree[0];
		tree.setName(line);

		while ((line = reader.readLine()) != null) {
			String[] kv = line.replaceFirst(".$", "").split(":", 2);
			leaves.put(kv[0], new PantherID(kv[1].replace('=', ':')));
		}
		
		/*
		for (Map.Entry<String,PantherID> kv : leaves.entrySet()) {
			tree.getNode(kv.getKey()).setName(kv.getValue().toString());
		}
		*/
		
		this.close();
	}
	
	public String toString() {
		return "[title:" + tree.getName() + ']' + tree.toString();
	}
	
	/**
	 * @return The name of the Panther cluster.
	 */
	public String getName() {
		return tree.getName();
	}
	
	/**
	 * Tells if the file read from is gzipped or not.
	 * @return True if it thinks the file is compressed, false if not.
	 */
	boolean gzipped()
	{
		return file.getName().endsWith(".gz");
	}
	
	/**
	 * 
	 * @return A BufferedReader class of the file.
	 * @throws IOException
	 */
	BufferedReader open() throws IOException
	{
		FileInputStream fis = new FileInputStream(file);
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
	
	/**
	 * 
	 * @return a collection of PantherID classes that are the leaves of the Panther tree.
	 */
	public Collection<PantherID> getLeaves() {
		return leaves.values();
	}
	
	public static void main(String[] args) throws IOException {
		UniProtSpecies.debug = true;
		TreeFileFilter tff = new TreeFileFilter();
	
		Collection<File> files = new TreeSet<File>();
		for (String pathname : args) {
			File f = new File(pathname);
			if (f.isDirectory()) {
				files.addAll(Arrays.asList(f.listFiles(tff)));
			} else {
				files.add(f);
			}	
		}
		
		if (files.size() == 1) {
			System.err.println("Will load " + files.iterator().next());
		} else {
			System.err.println("Will load " + files.size() + " files");
		}
		
		PhyloTreeLoader ptl = new PhyloTreeLoader();
		for (File file : files) {
			ptl.setSource(file);
			ptl.loadFromFile();
			for (PantherID id : ptl.getLeaves()) {
				//System.out.println(id.toString() + " => " + id.getTaxonNode());
				id.getTaxonNode();
			}
			//System.out.println(tree.toString());
		}
		
		System.err.println("All Done.");
	}


	static public class TreeFileFilter implements FileFilter {
		private final String[] suffixes = { ".tree", ".tree.gz" };
		
		public boolean accept(File pathname) {
			if (pathname.isDirectory()) {
				return false;
			}
			
			for (String suffix : suffixes) {
				if (pathname.getName().endsWith(suffix)) {
					return true;
				}
			}
			
			return false;
		}
	}
	
}