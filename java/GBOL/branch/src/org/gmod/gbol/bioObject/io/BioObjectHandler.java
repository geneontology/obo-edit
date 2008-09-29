package org.gmod.gbol.bioObject.io;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.gmod.gbol.bioObject.Gene;
import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.io.SimpleObjectIOInterface;

/** I/O handler for processing Bio objects.
 * 
 * @author elee
 *
 */

public class BioObjectHandler implements BioObjectIOInterface {

	private BioObjectConfiguration conf;
	private SimpleObjectIOInterface handler;
	
	/** Constructor.
	 * 
	 * @param conf - File name of configuration containing mapping information
	 * @param handler - SimpleObjectIOInterface that handles the underlying simple object data source
	 */
	public BioObjectHandler(String conf, SimpleObjectIOInterface handler)
	{
		this(new BioObjectConfiguration(conf), handler);
	}

	/** Constructor.
	 * 
	 * @param conf - Configuration containing mapping information
	 * @param handler - SimpleObjectIOInterface that handles the underlying simple object data source
	 */
	public BioObjectHandler(BioObjectConfiguration conf, SimpleObjectIOInterface handler)
	{
		this.conf = conf;
		this.handler = handler;
	}
	
	public Collection<Gene> getGenes() throws Exception {
		Collection<CVTerm> cvterms = conf.getCVTermsForClass("Gene");
		List<Gene> genes = new ArrayList<Gene>();
		for (CVTerm cvterm : cvterms) {
			for (Feature gene : handler.getFeaturesByCVTerm(cvterm)) {
				genes.add(new Gene(gene, conf));
			}
		}
		return genes;
	}

}
