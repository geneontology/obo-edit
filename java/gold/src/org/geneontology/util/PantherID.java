package org.geneontology.util;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import java.util.Set;
import java.util.Collection;
import java.util.TreeSet;
import java.util.LinkedHashSet;

import java.util.Arrays;

public class PantherID implements Comparable<PantherID> {
	final static String[] refG = { "DANRE", "ARATH", "CHICK", "RAT", "MOUSE", "DICDI", "YEAST", "DROME", "CAEEL", "HUMAN", "SCHPO", "ECOLI" };
	
	static Set<String> needSpecies = new TreeSet<String>();
	static Map<String,UniProtSpecies> haveSpecies = new TreeMap<String,UniProtSpecies>();
	
	private String pantherID;
	private String speciesCode;
	private Collection<String> IDs;
	
	public int compareTo(PantherID o) {
		return pantherID.compareTo(o.getPantherID());
	}
	
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
	
	public String getPantherID() {
		return pantherID;
	}
	
	public String toString() {
		return pantherID;
	}
	
	/*
	public int getNcbiTaxonId() {
		int out = -1;
		try {
			out = getTaxonNode();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.exit(1);
		}
		return out;
	}
	*/
	
	public int getTaxonNode() throws IOException {
		flushNeededSpecies();
		return haveSpecies.get(speciesCode).getTaxonNode();
	}
	
	public Collection<String> getIDs() {
		return this.IDs;
	}
	
	public String getSpeciesCode() {
		return this.speciesCode;
	}
	
	public boolean isRefG(){
		for (String code : refG) {
			if (code.contentEquals(this.speciesCode)) {
				return true;
			}
		}
		return false;
	}
	

}
