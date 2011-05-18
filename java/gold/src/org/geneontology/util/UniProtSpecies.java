package org.geneontology.util;

import java.io.*;
import java.util.*;


/**
 * Access species information stored in UniProt's <a
 * href="http://www.uniprot.org/docs/speclist">speclist.txt</a> file.
 */
public class UniProtSpecies
{
	public static boolean debug = false;
	
	/*
    protected UniProtSpecies() {
    }
    */

    /**
     * This is called by the Factory function to create objects.
     * @param code UniProt species code.
     * @param kingdom See <a href="http://www.uniprot.org/docs/speclist">speclist.txt</a> for values used.
     * @param taxon_node NCBI species id.
     */
    protected UniProtSpecies(String code, char kingdom, int taxon_node) {
    	this.code       = code;
    	this.kingdom    = kingdom;
    	this.taxon_node = taxon_node;
    	this.comment    = new TreeSet<String>();
    }

    /**
     * UniProt species code.
     */
    private String code;
    
    /**
     * UniProt kingdom, see UniProt definition for detail. Not currently used.
     */
    private char kingdom;
    
    /**
     * NCBI species id.
     */
    private int taxon_node;
    
    /**
     * Collection of other comments in the speclist.txt file, not currently used.
     */
    private Collection<String> comment;
    
    protected void addComment(String comment) {
    	this.comment.add(comment);
    }

    /**
     * @return A string representation of object.
     */
    public String toString() {
    	return "code:" + this.code                  +
    	" kingdom:"    + this.kingdom               +
    	" taxon_node:" + String.valueOf(taxon_node) +
    	' '            + comment;
    }
    
    /**
     * @return The UniProt Species code.
     */
    public String getCode() {
    	return code;
    }
    
    /**
     * @return The NCBI Taxon Node id.
     */
    int getTaxonNode() {
    	return taxon_node;
    }

    /**
     * Scrubs the input, pops it in a Set, and sends it off to the other factory(Set<String>) function.
     * @param codes An array of UniProt species codes.
     * @return
     * @throws java.io.IOException
     */
    static public List<UniProtSpecies> factory(String codes[]) throws java.io.IOException
    {
    	// chuck everything in a Set to remove dups.
    	Set<String> codeSet = new TreeSet<String>();
    	for (String code : codes) {
    		codeSet.add(code.trim().toUpperCase());
    	}
    	return factory(codeSet);
    }
 
  /**
   * Returns a list of UniProtSpecies objects. This is used instead of a constructor so the speclist.txt file need only be scanned once. Any codes not found are printed to standard error.
   * 
   * @param codeSet A set of UniProt species objects in the order found it the specilist.txt file.
   * @return Array of UniProt species codes.
   * @throws java.io.IOException If it has problems accessing the speclist.txt file.
   */
    static public List<UniProtSpecies> factory(Set<String> codeSet) throws java.io.IOException {
    	if (debug) {
    		System.err.println("Opening speclist.txt");
    	}
    	BufferedReader br = new BufferedReader(new InputStreamReader(UniProtSpecies.class.getResourceAsStream("/speclist.txt"), "US-ASCII"));

    	UniProtSpecies have = null; // the current item we are loading. If null, none.
    	// We want the output to be the order found in the UniProtSpecies file, so we use a list.
    	List<UniProtSpecies> output = new Vector<UniProtSpecies>();
    	String line;
    	while ((line = br.readLine()) != null) {
    		try {
    			// Generally only full if we are at the start of a new field.
    			String code = line.substring(0,5).trim();

    			if (code.isEmpty()) {
    				if (have != null) {
    					have.addComment(line.substring(16));
    				}
    			}  else if (codeSet.contains(code)) { // we found a code we are looking for.
    				if (debug) {
    					System.err.println("loading " + code);
    				}
    				char kingdom = line.charAt(6);
    				int taxon_node = Integer.decode(line.substring(8, 14).trim()).intValue();
    				have = new UniProtSpecies(code, kingdom, taxon_node);
    				have.addComment(line.substring(16));

    				output.add(have);
    				codeSet.remove(code);
    			} else {
    				have = null;
    			}

    			if (codeSet.isEmpty() && (have == null)) {
    				br.close();
    				return output;
    			}

    		} catch (Exception e) {
    			have = null;
    			// If we got nothing we just skip to the next line.
    		}
    	}

    	// we only ever get here if codeSet still has items in it.
    	br.close();
    	System.out.println(codeSet);
    	return output;
    }

    /**
     * 
     * @param args Takes an array of UniProt species codes, prints out a summary of the entry.
     * @throws java.io.IOException
     */
    public static void main(String[] args) throws java.io.IOException {
    	for (UniProtSpecies ups : factory(args)) {
    		System.out.println(ups.toString());
    	}
    }

    
}