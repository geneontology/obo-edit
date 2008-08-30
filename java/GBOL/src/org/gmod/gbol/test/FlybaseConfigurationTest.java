package org.gmod.gbol.test;

import org.gmod.gbol.simpleObject.CV;
import org.gmod.gbol.simpleObject.CVTermRelationship;
import org.gmod.gbol.simpleObject.DB;
import org.gmod.gbol.simpleObject.CVTerm;

public class FlybaseConfigurationTest extends AbstractGBOLTest{

	public FlybaseConfigurationTest(String name) {
		super(name);
		try {

			if (this.sf == null){
				this.configureConnection("dbConnectConfigurations/flybaseConfig.cfg.xml");
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
		
		CV cv = cvterm.getCv();
		cv.setName("newName");
		this.sf.getCurrentSession().save(cvterm);
		this.sf.getCurrentSession().getTransaction()
	}
	
}