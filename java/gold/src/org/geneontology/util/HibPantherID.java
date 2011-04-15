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
		boolean out = false;
		String cls = be.getNcbiTaxonId();
		
		try {
			out = (Integer.valueOf(cls.substring(cls.indexOf(':') + 1)) == this.getTaxonNode());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return out;
	}
		
	public Collection<Bioentity> bioentityMatch() {
		Collection<Bioentity> out = new Vector<Bioentity>();
		
		for (String id : this.getIDs()) {
			Bioentity match = gof.getBioentity(id);
			if (null != match) {
				if (this.sameSpecies(match)) {
					out.add(gof.getBioentity(id));
				} else {
					System.out.println("NCBI Taxon ID Mismatch for " + this);
				}
			}
		}
		
		// TODO: if out.isEmpty() then try harder
		
		return out;
	}
}
