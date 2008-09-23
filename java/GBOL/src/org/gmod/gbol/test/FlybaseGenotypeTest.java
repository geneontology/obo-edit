package org.gmod.gbol.test;

import org.gmod.gbol.simpleObject.CVTermRelationship;
import org.gmod.gbol.simpleObject.CVTerm;

public class FlybaseGenotypeTest extends AbstractGBOLHibernateTest{

	public FlybaseGenotypeTest(String name) {
		super(name);
		try {
			if (this.sf == null){
				this.configureConnection("testSupport/flybaseConfig.cfg.xml");
			}
			assertTrue(true);
		} catch (Exception e) {
			assertTrue(false);
			e.printStackTrace();
		}
	}
	
	public void testAllelQuery(){
		System.out.println("YO!");
		
	}
	
}