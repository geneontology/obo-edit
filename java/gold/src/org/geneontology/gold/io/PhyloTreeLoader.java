package org.geneontology.gold.io;

import java.io.*;
import java.util.zip.GZIPInputStream;

import java.util.Arrays;
import java.util.Collection;
import java.util.TreeSet;

/**
 * loads phylogenetic trees into database
 * 
 *  TBD - directly from NHX file or from Panther
 *
 */
public class PhyloTreeLoader {
	private File file;

	private BufferedReader reader;
	
	PhyloTreeLoader(File file) {
		this.file = file;
	}
	
	boolean gzipped()
	{
		return file.getName().endsWith(".gz");
	}
	
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
	
	void close() throws IOException
	{
		reader.close();
	}
	
	public static void main(String[] args) throws IOException {
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
			tree.open();
			tree.close();
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