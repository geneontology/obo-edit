package org.gmod.gbol.bioObject;

import java.util.ArrayList;
import java.util.Collection;

import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureRelationship;

/** Wrapper class representing a transcript.
 * 
 * @author elee
 *
 */

public class Transcript extends AbstractSingleLocationBioObject {

	/** Constructor.
	 * 
	 * @param feature - Feature object that this class wraps
	 * @param conf - Configuration containing mapping information
	 */
	public Transcript(Feature feature, BioObjectConfiguration conf)
	{
		super(feature, conf);
	}
	
	/** Retrieve all the exons associated with this gene.  Uses the configuration to determine
	 *  which children are exons.  Exon objects are generated on the fly.  The collection
	 *  will be empty if there are no exons associated with the transcript.
	 *  
	 * @return Collection of exons associated with this transcript
	 */

	public Collection<Exon> getExons()
	{
		Collection<Exon> exons = new ArrayList<Exon>();
		Collection<CVTerm> partOfCvterms = conf.getCVTermsForClass("PartOf");
		Collection<CVTerm> exonCvterms = conf.getCVTermsForClass("Exon");
		
		for (FeatureRelationship fr : feature.getChildFeatureRelationships()) {
			if (!partOfCvterms.contains(fr.getType())) {
				continue;
			}
			if (!exonCvterms.contains(fr.getSubjectFeature().getType())) {
				continue;
			}
			exons.add(new Exon(fr.getSubjectFeature(), conf));
		}
		return exons;
	}

}
