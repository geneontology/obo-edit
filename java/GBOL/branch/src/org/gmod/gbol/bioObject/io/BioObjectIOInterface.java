package org.gmod.gbol.bioObject.io;

import java.util.Collection;

import org.gmod.gbol.bioObject.Gene;

public interface BioObjectIOInterface {

	/** Get all genes from data source.  Returns any feature that is configured to be
	 *  mapped to Gene.
	 *  
	 * @return Collection of Gene objects - empty if no objects are found
	 * @throws Exception if an error has occurred fetching genes
	 */
	public Collection<Gene> getGenes() throws Exception;
}
