package org.gmod.gbol.bioObject.io;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.gmod.gbol.bioObject.AbstractBioFeature;
import org.gmod.gbol.bioObject.Gene;
import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.bioObject.util.BioObjectUtil;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;
import org.gmod.gbol.simpleObject.FeatureRelationship;
import org.gmod.gbol.simpleObject.SimpleObjectIteratorInterface;
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
	
	public Collection<AbstractBioFeature> getAllFeatures() throws Exception
	{
		Collection<AbstractBioFeature> feats = new ArrayList<AbstractBioFeature>();
		for (Feature f : handler.getAllFeatures()) {
			feats.add((AbstractBioFeature)BioObjectUtil.createBioObject(f, conf));
		}
		return feats;
	}
	
	public Collection<Gene> getAllGenes() throws Exception
	{
		Collection<CVTerm> cvterms =
			conf.getCVTermsForClass(BioObjectUtil.stripPackageNameFromClassName(Gene.class.getName()));
		List<Gene> genes = new ArrayList<Gene>();
		for (CVTerm cvterm : cvterms) {
			for (Feature gene : handler.getFeaturesByCVTerm(cvterm)) {
				genes.add(new Gene(gene, conf));
			}
		}
		return genes;
	}
	
	public void write(Collection<? extends AbstractBioFeature> features)
	{
		for (AbstractBioFeature f : features) {
			SimpleObjectIteratorInterface i = f.getWriteableSimpleObjects(conf);
			try {
				handler.write(i);
			}
			catch (Exception e) {
				throw new RuntimeException("Error writing object: " + e.getMessage());
			}
			
			/*
			while (i.hasNext()) {
				AbstractSimpleObject o = i.next();
				*/
				/*
				if (o instanceof Feature) {
					System.out.println(toString((Feature)o));
				}
				else {
					System.out.println(toString((FeatureRelationship)o));
				}
				*/
				/*
				System.out.println(o.getClass().getName());
				SimpleObjectIteratorInterface j = o.getWriteableObjects();
				while (j.hasNext()) {
					AbstractSimpleObject o2 = j.next();
					System.out.println("\t" + o2.getClass().getName());
				}
				*/
			/*
			}
			*/
		}
	}
	
	private String toString(FeatureRelationship fr)
	{
		return String.format("\t%s%n\t%s", toString(fr.getSubjectFeature()), toString(fr.getObjectFeature()));
	}
	
	private String toString(Feature f)
	{
		FeatureLocation fl = f.getFeatureLocations().iterator().next();
		String retVal = String.format("%s\t%s\t%d\t%d", f.getName(), f.getType().getName(), fl.getFmin(), fl.getFmax());
		if (fl.getSourceFeature() != null) {
			retVal += "\n\t\t[srcfeature]" + toString(fl.getSourceFeature());
		}
		return retVal;
	}
}
