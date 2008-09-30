package org.gmod.gbol.test;

import java.util.Collection;

import org.apache.log4j.PropertyConfigurator;
import org.gmod.gbol.bioObject.AbstractSingleLocationBioObject;
import org.gmod.gbol.bioObject.Exon;
import org.gmod.gbol.bioObject.Gene;
import org.gmod.gbol.bioObject.Transcript;
import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.bioObject.io.BioObjectHandler;
import org.gmod.gbol.simpleObject.io.SimpleObjectIOInterface;
import org.gmod.gbol.simpleObject.io.impl.HibernateHandler;

import junit.framework.TestCase;

public class BioObjectHandlerTest extends TestCase {

	private BioObjectHandler handler;
	
	public BioObjectHandlerTest() throws Exception
	{
		PropertyConfigurator.configure("testSupport/log4j.properties");
		BioObjectConfiguration conf = new BioObjectConfiguration("testSupport/gbolOne.mapping.xml");
		SimpleObjectIOInterface h = new HibernateHandler("testSupport/gbolOne.cfg.xml");
		handler = new BioObjectHandler(conf, h);
	}
	
	public void testGetGenes() throws Exception
	{
		Collection<Gene> genes = handler.getGenes();
		assertEquals(genes.size(), 2);
		for (Gene gene : genes) {
			printFeatureInfo(gene, 0);
			Collection<Transcript> transcripts = gene.getTranscripts();
			assertEquals(transcripts.size(), 1);
			for (Transcript transcript : transcripts) {
				printFeatureInfo(transcript, 1);
				Collection<Exon> exons = transcript.getExons();
				for (Exon exon : exons) {
					printFeatureInfo(exon, 2);
				}
			}
		}
	}
	
	private void printFeatureInfo(AbstractSingleLocationBioObject feature, int indent)
	{
		for (int i = 0; i < indent; ++i) {
			System.out.print("\t");
		}
		System.out.printf("%s\t(%d,%d)%n", feature.getName(), feature.getFeatureLocation().getFmin(),
				feature.getFeatureLocation().getFmax());
	}

}
