package org.geneontology.gold.io;

import java.io.*;
//import java.util.zip.GZIPInputStream;

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
	
	public static void main(String[] args) {
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