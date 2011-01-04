package org.geneontology.util;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import java.util.Set;
import java.util.TreeSet;
import java.util.LinkedHashSet;

import java.util.Arrays;

public class PantherID {
	static Set<String> needSpecies = new TreeSet<String>();
	static Map<String,UniProtSpecies> haveSpecies = new TreeMap<String,UniProtSpecies>();
	
	String pantherID;
	String speciesCode;
	Set<String> IDs;
	
	public PantherID(String pantherID) {
		this.pantherID = pantherID;
		String codeIDs[] = pantherID.split("\\|", 2);
		
		// work around mislabeled item in Panther
		if (codeIDs[0].contentEquals("FUGRU")) {
			this.speciesCode = "TAKRU";
		} else {
			this.speciesCode = codeIDs[0];
		}
		IDs = new LinkedHashSet<String>(Arrays.asList(codeIDs[1].split("\\|")));
		
		if (!haveSpecies.containsKey(this.speciesCode)) {
			needSpecies.add(this.speciesCode);
		}
	}
	
	protected void flushNeededSpecies() throws IOException {
		if (needSpecies.isEmpty()) {
			return;
		}
		
		for (UniProtSpecies got : UniProtSpecies.factory(needSpecies)) {
			haveSpecies.put(got.getCode(), got);
		}
		
		needSpecies.clear();
	}
	
	public String toString() {
		return pantherID;
	}
	
	public int getTaxonNode() throws IOException {
		flushNeededSpecies();
		return haveSpecies.get(speciesCode).getTaxonNode();
	}
}
