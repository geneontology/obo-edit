package org.gmod.gbol.bioObject.io;

import java.util.Collection;

import org.gmod.gbol.bioObject.AbstractBioFeature;
import org.gmod.gbol.bioObject.Gene;

/** I/O interface for all Bio object layer I/O handlers.
 * 
 * @author elee
 *
 */
public interface BioObjectIOInterface {

	/**	Get all features from data source.  Returns all features mapped to their corresponding
	 *  data types.
	 *  
	 * @return Collection of AbstraBioFeature objects - empty if no objects are found
	 * @throws Exception if an error has occurred fetching features
	 */
	public Collection<AbstractBioFeature> getAllFeatures() throws Exception;
	
	/** Get all genes from data source.  Returns any feature that is configured to be
	 *  mapped to Gene.
	 *  
	 * @return Collection of Gene objects - empty if no objects are found
	 * @throws Exception if an error has occurred fetching genes
	 */
	public Collection<Gene> getAllGenes() throws Exception;

	/** Write the passed features to the data source.
	 * 
	 * @param features - Collection of AbstractBioFeature objects to be written
	 */
	public void write(Collection<? extends AbstractBioFeature> features);
}
