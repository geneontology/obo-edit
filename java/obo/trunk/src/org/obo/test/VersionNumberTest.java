package org.obo.test;

import org.obo.util.VersionNumber;

import junit.framework.TestCase;

public class VersionNumberTest extends TestCase  {


	public void testVersionNumbers() throws Exception {
		VersionNumber v1 = new VersionNumber("2.1-beta1");
		
		assertEquals(2, v1.getMajorVersion());
		assertEquals(1, v1.getMinorVersion());
		assertEquals(1, v1.getBetaVersion());
		assertEquals(0, v1.getRCVersion());
		
		VersionNumber v2 = new VersionNumber("2.3-b4");
		
		assertEquals(2, v2.getMajorVersion());
		assertEquals(3, v2.getMinorVersion());
		assertEquals(4, v2.getBetaVersion());
		assertEquals(0, v2.getRCVersion());
		
		VersionNumber v3 = new VersionNumber("2.4-rc2");
		
		assertEquals(2, v3.getMajorVersion());
		assertEquals(4, v3.getMinorVersion());
		assertEquals(0, v3.getBetaVersion());
		assertEquals(2, v3.getRCVersion());
		
	}
}
