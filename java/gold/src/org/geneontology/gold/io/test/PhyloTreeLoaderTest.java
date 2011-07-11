package org.geneontology.gold.io.test;

import java.io.File;
import java.util.List;

import org.geneontology.gold.io.PhyloTreeLoader;



public class PhyloTreeLoaderTest extends junit.framework.TestCase {
	PhyloTreeLoader ptl;
	
	public void setUp(){
		ptl = new PhyloTreeLoader();
		File paintScrapped = new File("test_resources/PTHR10000.tree");
		ptl.setSource(paintScrapped);
	}
	
	public void testLoad() throws Exception{
		ptl.loadThrow();
	}
}
