package org.gmod.gbol.test;

import java.util.Collection;

import org.gmod.gbol.simpleObject.CV;
import org.gmod.gbol.simpleObject.CVTermRelationship;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureRelationship;
import org.hibernate.Query;

public class FlybaseConfigurationTest extends AbstractGBOLHibernateTest{

	public FlybaseConfigurationTest(String name) {
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
	
	public void testConfigure(){
		
		CVTerm cvterm = (CVTerm) this.sf.getCurrentSession().get(CVTerm.class, 30);
		System.out.println("Parent: " + cvterm.getName());
		for (CVTermRelationship cvtr : cvterm.getChildCVTermRelationships()){
			System.out.println(cvtr.getSubjectCVTerm().getName() + " " + cvtr.getType().getName() + " " + cvtr.getObjectCVTerm().getName());
		}
		
		
		
		
	}
	
}