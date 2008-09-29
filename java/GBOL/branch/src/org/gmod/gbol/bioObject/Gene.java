package org.gmod.gbol.bioObject;

import java.util.ArrayList;
import java.util.Collection;

import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureRelationship;

/** Wrapper class representing a gene.
 * 
 * @author elee
 *
 */

public class Gene extends AbstractSingleLocationBioObject {

	/** Constructor.
	 * 
	 * @param feature - Feature object that this class wraps
	 * @param conf - Configuration containing mapping information
	 */
	public Gene(Feature feature, BioObjectConfiguration conf)
	{
		super(feature, conf);
	}

	/** Retrieve all the transcripts associated with this gene.  Uses the configuration to determine
	 *  which children are transcripts.  Transcript objects are generated on the fly.  The collection
	 *  will be empty if there are no transcripts associated with the gene.
	 *  
	 * @return Collection of transcripts associated with this gene
	 */
	public Collection<Transcript> getTranscripts()
	{
		Collection<Transcript> transcripts = new ArrayList<Transcript>();
		Collection<CVTerm> partOfCvterms = conf.getCVTermsForClass("PartOf");
		Collection<CVTerm> transcriptCvterms = conf.getCVTermsForClass("Transcript");
		
		for (FeatureRelationship fr : feature.getChildFeatureRelationships()) {
			if (!partOfCvterms.contains(fr.getType())) {
				continue;
			}
			if (!transcriptCvterms.contains(fr.getSubjectFeature().getType())) {
				continue;
			}
			transcripts.add(new Transcript(fr.getSubjectFeature(), conf));
		}
		return transcripts;
	}

}
