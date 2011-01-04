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
public class PhyloTreeLoader {
	private File file;
	private BufferedReader reader;
	private Phylogeny tree;
	Map<String,PantherID> leaves;
	
	static NHXParser parser = null;
	
	/**
	 * 
	 * @param file - File object painting to file produced by org.paint.util.PaintScraper class. Can be gzipped.
	 * @throws IOException If it has problems accessing the file.
	 */
	PhyloTreeLoader(File file) throws IOException {
		this.file = file;
		this.open();
		
		if (parser == null) {
			parser = new NHXParser();
		}
		
		// get the title and remove the prefix "[title:" and "]"
		String line = reader.readLine().substring(7).replaceFirst(".$", "");

		parser.setSource(reader.readLine());
		Phylogeny oneTree[] = parser.parse();
		tree = oneTree[0];
		tree.setName(line);

		leaves = new TreeMap<String,PantherID>();
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
		
		for (File file : files) {
			PhyloTreeLoader tree = new PhyloTreeLoader(file);
			for (PantherID id : tree.getLeaves()) {
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