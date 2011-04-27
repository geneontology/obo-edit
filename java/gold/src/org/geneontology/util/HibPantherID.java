package org.geneontology.util;
// If org.geneontology.util.PantherID would go into go-perl, this would go into go-perl-db

import java.util.Collection;
import java.util.Vector;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.GafObjectsFactory;

public class HibPantherID extends PantherID {
	private static GafObjectsFactory gof;
	
	public HibPantherID(String pid) {
		super(pid);
		
		gof = new GafObjectsFactory();
	}
		
	boolean sameSpecies(Bioentity be) {
		String cls = be.getNcbiTaxonId();
		
		for (int taxonNode : this.getTaxonNodes()) {
			if (Integer.valueOf(cls.substring(cls.indexOf(':') + 1)) == taxonNode) {
				return true;
			}
		}
		return false;
	}
		
	public Collection<Bioentity> bioentityMatch() {
		Collection<Bioentity> out = new Vector<Bioentity>();
		
		// first try the ids that we have
		for (String id : this.getIDs()) {
			Bioentity match = gof.getBioentity(id);
			if (null != match) {
				if (this.sameSpecies(match)) {
					out.add(match);
				} else {
					System.out.println("NCBI Taxon ID Mismatch for " + this);
				}
			}
		}

		if (out.isEmpty()) {
			// here we try altered ids
			for (String id : this.getIDguesses()) {
				Bioentity match = gof.getBioentity(id);
				if (null != match) {
					if (this.sameSpecies(match)) {
						out.add(match);
					} else {
						// Lets not be no verbose when it come to the guessed IDs
						//System.out.println("NCBI Taxon ID Mismatch for " + this);
					}
				}
			}
		}
		
		return out;
	}


}
