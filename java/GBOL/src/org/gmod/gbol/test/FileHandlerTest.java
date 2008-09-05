package org.gmod.gbol.test;

import java.io.IOException;

import org.gmod.gbol.io.FileHandler;
import org.gmod.gbol.io.impl.GFF3Handler;

import junit.framework.TestCase;

public class FileHandlerTest extends TestCase {
	
	FileHandler fileHandler;
	private final String filePath = "testSupport/exampleGFF3.gff"; 
	
	public void testConstructor(){
		try {
			this.fileHandler = new GFF3Handler(filePath,null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}