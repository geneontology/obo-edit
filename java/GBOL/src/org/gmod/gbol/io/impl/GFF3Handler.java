package org.gmod.gbol.io.impl;

import java.io.IOException;
import java.util.Collection;

import org.gmod.gbol.io.FileHandler;
import org.gmod.gbol.io.IOConfiguration;
import org.gmod.gbol.io.IOInterface;
import org.gmod.gbol.simpleObject.AbstractSimpleObject;


public class GFF3Handler extends FileHandler implements IOInterface {

	public GFF3Handler(String filePath, IOConfiguration configuration) throws IOException {
		super(filePath, configuration);
	}

	public Collection<AbstractSimpleObject> readAll() {
		// TODO Auto-generated method stub
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