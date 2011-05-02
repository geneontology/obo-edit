package org.geneontology.util;
// If org.geneontology.util.PantherID would go into go-perl, this would go into go-perl-db

import java.util.List;
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

	List<Bioentity> bioentities = null;
	public List<Bioentity> bioentityMatch() {
		if (null == bioentities) {
			bioentities = new Vector<Bioentity>();
		
			// first try the ids that we have
			for (String id : this.getIDs()) {
				Bioentity match = gof.getBioentity(id);
				if (null != match) {
					if (this.sameSpecies(match)) {
						bioentities.add(match);
					} else {
						System.out.println("NCBI Taxon ID Mismatch for " + this);
					}
				}
			}

			if (bioentities.isEmpty()) {
				// here we try altered ids
				for (String id : this.getIDguesses()) {
					Bioentity match = gof.getBioentity(id);
					if (null != match) {
						if (this.sameSpecies(match)) {
							bioentities.add(match);
						} else {
							// Lets not be no verbose when it come to the guessed IDs
							//System.out.println("NCBI Taxon ID Mismatch for " + this);
						}
					}
				}
			}
		}
		
		return bioentities;
	}

	public Bioentity bioentity(){
		List<Bioentity> pick = bioentityMatch();
		Bioentity out = null;
		int size = pick.size();
		
		if (1 == size) {
			out = pick.get(0);
		} else if (size > 1) {
			switch(getTaxonNode()) {
			case 6239:  // Caenorhabditis elegans
				out = pick("WB");
				break;
			case 10116: // Rattus norvegicus
				out = pick("RGD"); //new String[]  { "RGD", "UniProtKB", "ENSEMBL"});
				break;
			default:
				System.err.println(this.getSpeciesCode() + " => " + pick);
				System.exit(1);
				//out = pick.get(0); // TODO: use logic here, not just take the first
				//System.err.println(getPantherID() + " => " + out + " from " + pick);
			}
		}
		
		return out;
	}
	
	protected Bioentity pick(String find) {
		return pick(new String[] { find });
	}
	
	protected Bioentity pick(String[] find) {	
		for (String f : find) {
			for (Bioentity b : bioentityMatch()) {
				String id[] = b.getId().split(":", 2);
				if (f.equalsIgnoreCase(id[0])) {
					System.err.println("Picked " + b + " from " + bioentityMatch());
					return b;
				}
			}
		}
		System.err.println("no id found for " + bioentityMatch());
		System.exit(0);
		
		return null;
	}
	
}
