package org.gmod.gbol.io.impl;

import java.io.IOException;
import java.util.Collection;

import org.gmod.gbol.io.FileHandler;
import org.gmod.gbol.io.IOConfiguration;
import org.gmod.gbol.io.IOInterface;
import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.Organism;


public class GFF3Handler extends FileHandler implements IOInterface {

	Organism organism;
	
	public GFF3Handler(String filePath, IOConfiguration configuration, Organism o) throws IOException {
		super(filePath, configuration);
		this.organism = o;
	}
	
	

	public Collection<AbstractSimpleObject> readAll() {
		// TODO Auto-generated method stub
		List<AbstractSimpleObject> object = new 
		return null;
	}

	public boolean write(AbstractSimpleObject simpleObject) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean write(Collection<AbstractSimpleObject> simpleObjects) {
		// TODO Auto-generated method stub
		return false;
	}
	
}