package org.gmod.gbol.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.gmod.gbol.io.FileHandler;
import org.gmod.gbol.io.impl.GFF3Handler;
import org.gmod.gbol.simpleObject.Feature;

import junit.framework.Assert;
import junit.framework.TestCase;

public class GFF3HandlerTest extends TestCase {
	
	FileHandler fileHandler;
	private final String filePath = "testSupport/exampleGFF3.gff"; 
	
	private final int EXPECTED_FEATURE_COUNT = 1723;
	
	@Override
	public void setUp(){
		try {
			this.fileHandler = new GFF3Handler(filePath);
		} catch (IOException e) {
			System.err.println("Unable to open " + filePath + " for GFF3HandlerTest support.");
			e.printStackTrace();
		}
	}
	
	@SuppressWarnings("unchecked")
	public void testGFF3Read(){
		Collection<Feature> features = new ArrayList<Feature>();
		try {
			features = (Collection<Feature>) this.fileHandler.readAll();
			Assert.assertTrue("Succsefully read " + filePath + ".", true);
		} catch (Exception e) {
			e.printStackTrace();
			Assert.assertTrue("Failed to read " + filePath + ".\nERROR: " + e.getMessage() + "\n",false);
		}
		
		Assert.assertTrue("Checking number of features.",(features.size()==EXPECTED_FEATURE_COUNT));
		System.out.println("Read " + features.size() + " features.");
	}
	
	
	
	
	
	
	
}