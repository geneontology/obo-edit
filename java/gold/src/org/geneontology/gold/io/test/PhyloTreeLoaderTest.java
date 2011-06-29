package org.geneontology.gold.io.test;

import java.io.File;
import java.util.List;

import org.geneontology.gold.io.PhyloTreeLoader;



public class PhyloTreeLoaderTest extends junit.framework.TestCase {
	PhyloTreeLoader ptl;
	List<File> tmpfiles;
	
	public void SetUp(){
		ptl = new PhyloTreeLoader();
		File paintScrapped = new File("test_resources/PTHR10000.tree");
		ptl.setSource(paintScrapped);
	}
	
	// modified of PhyloTreeLoader.load()
	public void test(){
		// loades sources into memory
		ptl.loadFromFile();
		
		// writeTSV() marks the tmpfiles as to be deleted on exit, so they don't need to be cleaned up
		List<File> tmpfiles = ptl.writeTSV();

		// commented out as ptl.hib() writes to the database
		/*
		try {
			ptl.hib(tmpfiles);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
		*/
	}
}
