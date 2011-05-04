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

	private List<Bioentity> bioentities = null;
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
							System.out.println("NCBI Taxon ID Mismatch for " + this);
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
				out = pick(new String[]  { "RGD", "UniProtKB", "ENSEMBL"});
				break;
			default:
				System.err.println(this.getSpeciesCode() + " => " + pick);
				System.exit(1);
			}
		}
		
		// if size == 0 return null
		return out;
	}
	
	protected Bioentity pick(String find) {
		for (Bioentity b : bioentityMatch()) {
			String id[] = b.getId().split(":", 2);
			if (find.equalsIgnoreCase(id[0])) {
				//System.err.println("Matched " + bioentityMatch() + " to " + b);
				return b;
			}
		}
		return null;
	}

	protected Bioentity pick(String[] find) {	
		for (String f : find) {
			Bioentity got = pick(f);
			if (null != got) {
				return got;
			}
		}
		System.err.println("no id picked for " + bioentityMatch());
		System.exit(0);
		
		return null;
	}
	
}
