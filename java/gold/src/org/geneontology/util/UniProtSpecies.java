package org.geneontology.util;

import java.io.*;
import java.util.*;


/**
 * Access species information stored in UniProt's <a
 * href="http://www.uniprot.org/docs/speclist">speclist.txt</a> file.
 */
public class UniProtSpecies
{
	/*
    protected UniProtSpecies() {
    }
    */

    /**
     * This is called by the Factory function to create objects.

     * @param code UniProt species code.

     * @param kingdom See <a
     * href="http://www.uniprot.org/docs/speclist">speclist.txt</a>
     * for values used.

     * @param taxon_node NCBI species id.

     */
    protected UniProtSpecies(String code, char kingdom, int taxon_node) {
    	this.code       = code;
    	this.kingdom    = kingdom;
    	this.taxon_node = taxon_node;
    	this.comment    = new TreeSet<String>();
    }

    String code;
    char kingdom;
    int taxon_node;
    Collection<String> comment;
    
    public void addComment(String comment) {
    	this.comment.add(comment);
    }

    /**
     * Returns a string representation of object.
     * @return A string representation of object.
     */
    public String toString() {
    	return "code:" + this.code                  +
    	" kingdom:"    + this.kingdom               +
    	" taxon_node:" + String.valueOf(taxon_node) +
    	' '            + comment;
    }

    /**
     * Returns a list of UniProtSpecies objects. This is used instead
     * of a constructor so the speclist.txt file need only be scanned
     * once. Any codes not found are printed to standard error.

     @param codes Array of UniProt species codes.
     @return A list of UniProt species objects in the order found it
     the specilist.txt file.
       

     */
    static public List<UniProtSpecies> Factory(String codes[]) throws java.io.IOException
    {
    	Set<String> codeSet = new TreeSet<String>();
    	for (String code : codes) {
    		codeSet.add(code.trim().toUpperCase());
    	}

    	BufferedReader br = new BufferedReader(new InputStreamReader(UniProtSpecies.class.getResourceAsStream("/speclist.txt"), "US-ASCII"));

    	UniProtSpecies have = null;
    	// We want the output to be the order found in the
    	// UniProtSpecies file, so we use a list.
    	List<UniProtSpecies> output = new Vector<UniProtSpecies>();
    	String line;
    	while ((line = br.readLine()) != null) {
    		try {
    			String code = line.substring(0,5).trim();

    			if (code.isEmpty()) {
    				if (have != null) {
    					have.addComment(line.substring(16));
    				}
    			}  else if (codeSet.contains(code)) {
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

    public static void main(String[] args) throws java.io.IOException {
    	for (UniProtSpecies ups : Factory(args)) {
    		System.out.println(ups.toString());
    	}
    }

    
}