package org.gmod.gbol.io.impl;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.gmod.gbol.io.FileHandler;
import org.gmod.gbol.io.IOInterface;
import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.Organism;


public class GFF3Handler extends FileHandler implements IOInterface {

	private Organism organism;
	private String newlineCharacter;
	private String columnDelimiter;
	private ParseMode parseMode;
	private Map<String,Feature> features;
	private Map<String,Feature> sourceFeatures;
	
	
	private enum ParseMode {FEATURE,FASTA};
	
	public GFF3Handler(String filePath) throws IOException {
		super(filePath);
		this.newlineCharacter = "\n";
		this.columnDelimiter = "\t";
		this.parseMode = ParseMode.FEATURE;
		this.sourceFeatures =  new HashMap<String,Feature>();
		this.features =  new HashMap<String,Feature>();
	}

	public Organism getOrganism() {
		return organism;
	}

	public void setOrganism(Organism organism) {
		this.organism = organism;
	}

	public Collection<AbstractSimpleObject> readAll() throws Exception{
		
		
		
		this.openHandle();
		StringBuilder contents = this.readFileContents();
		
		String[] lines = contents.toString().split(this.newlineCharacter);
		for (int i=0;i<lines.length;i++){
			String line = lines[i].trim();
			if (line != ""){
				if (line.charAt(0) == '#'){
					
				} else {
					if (this.parseMode.equals(ParseMode.FEATURE)){
						try {
							Feature f = this.constructFeature(line);
						} catch (Exception e){
							System.err.println("Error parsing line " + i + " of GFF3 file " + this.getFilePath());
							System.err.println("Error message: " + e.getMessage());
							System.exit(-1);
						}
					} else {
						
					}
				}
			}
		}
		
		return null;
	}

	public boolean write(AbstractSimpleObject simpleObject) {
		System.err.println("Not implemented yet.");
		return false;
	}

	public boolean write(Collection<AbstractSimpleObject> simpleObjects) {
		System.err.println("Not implemented yet.");
		return false;
	}
	
	private void processCommentDirective(){
		
	}
	
	private Feature constructFeature(String line) throws Exception{
		String[] parts = line.split(this.columnDelimiter);
		if (parts.length != 9){
			throw new Exception("Unexpected number of columns (" + parts.length + "). Expecting 9.");
		}
		
		if (!this.sourceFeatures.containsKey(parts[0])){
			this.constructSourceFeature(parts[0]);
		}

		return null;
		
	}
	
	private void constructSourceFeature(String uniqueName){
		Feature sourceFeature = new Feature();
		sourceFeature.setUniqueName(uniqueName);
		
	}
	
}