package org.geneontology.gold.io.test;

import java.io.File;
import java.util.List;

import org.geneontology.gold.io.PhyloTreeLoader;



public class PhyloTreeLoaderTest extends junit.framework.TestCase {
	PhyloTreeLoader ptl;
	List<File> tmpfiles;
	
	public void setUp(){
		ptl = new PhyloTreeLoader();
		File paintScrapped = new File("test_resources/PTHR10000.tree");
		ptl.setSource(paintScrapped);
	}
	
	// modified from PhyloTreeLoader.load()
	public void testLoad() throws Exception{
		// loades sources into memory
		ptl.loadFromFile();
		
		// writeTSV() marks the tmpfiles as to be deleted on exit, so they don't need to be cleaned up
		tmpfiles = ptl.writeTSV();

		// Write to the database
		ptl.hib(tmpfiles);
	
	}
}
