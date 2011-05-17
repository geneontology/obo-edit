package org.geneontology.util;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import java.util.Set;
import java.util.Collection;
import java.util.TreeSet;
import java.util.LinkedHashSet;

import java.util.Arrays;

/**
 * An object to hold protein IDs as presented from Panther.
 * @author Sven Heinicke
 *
 */
public class PantherID implements Comparable<PantherID> {
	final static String[] refG = { "DANRE", "ARATH", "CHICK", "RAT", "MOUSE", "DICDI", "YEAST", "DROME", "CAEEL", "HUMAN", "SCHPO", "ECOLI" };
	
	private static Set<String> needSpecies = new TreeSet<String>();
	private static Map<String,UniProtSpecies> haveSpecies = new TreeMap<String,UniProtSpecies>();
	
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

	/**
	 * 
	 * @throws IOException
	 */
	protected void flushNeededSpecies() throws IOException {
		if (needSpecies.isEmpty()) {
			return;
		}
		
		for (UniProtSpecies got : UniProtSpecies.factory(needSpecies)) {
			haveSpecies.put(got.getCode(), got);
		}
		
		needSpecies.clear();
	}
	
	/**
	 * 
	 * @return The Panther ID.
	 */
	public String getPantherID() {
		return pantherID;
	}
	
	/**
	 * @return The Panther ID.
	 */
	public String toString() {
		return pantherID;
	}
	
	/**
	 * 
	 * @return Returns the NCBI Taxa id of the species of the Panther ID.
	 */
	public int getTaxonNode() {
		try {
			flushNeededSpecies();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return haveSpecies.get(speciesCode).getTaxonNode();
	}

	/**
	 * Some Panther ID specias get mapped to more then one bioentity in the gold database. This will return a list of possibilities. 
	 * @return Returns a list of possable NCBI Taxa ids.
	 */
	int[] getTaxonNodes() {
		int taxonNode = this.getTaxonNode();
		switch(taxonNode){
		case 83333: // ECOLI
			int[] out = { 83333, 511145 };
			return out;
		//break;
		}
		int[] out = new int[1];
		out[0] = taxonNode;
		return out;
	}
	
	/**
	 * Sometimes the Panther IDs are not exactly those used in gold. Returns a list of alternative IDs if we know any.
	 * @return List of ids to search the gold database with.
	 */
	public Collection<String> getIDguesses() {
		Collection<String> out = new LinkedHashSet<String>();

		switch(this.getTaxonNode()){
		case 83333: // ECOLI
			for (String id : this.getIDs()) {
				if (id.startsWith("ECOLI:")) {
					out.add(id.replaceFirst("ECOLI:", "EcoCyc:"));
//				} else if (id.startsWith("NCBI:")) {
//					out.add(id.replaceFirst("NCBI:", "RefSeq:"));
				}
			}
			break;
		}
		return out;
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
