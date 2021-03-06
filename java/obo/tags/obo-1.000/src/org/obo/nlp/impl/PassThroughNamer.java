package org.obo.nlp.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.LinkedObject;
import org.obo.nlp.Namer;

public class PassThroughNamer implements Namer {

	private Collection<Namer> namers;
	
	public PassThroughNamer() {
		namers = new LinkedList<Namer>();
	}

	public Collection<String> constructNames(LinkedObject lo) {
		Collection<String> names = new LinkedList<String>();
		for (Namer namer : namers) {
			names.addAll(namer.constructNames(lo));
		}
		return names;
	}

	public Collection<String> constructTextDefs(LinkedObject lo) {
		Collection<String> defs = new LinkedList<String>();
		for (Namer namer : namers) {
			defs.addAll(namer.constructTextDefs(lo));
		}
		return defs;
	}

	public Collection<Namer> getNamers() {
		return namers;
	}

	public void setNamers(Collection<Namer> namers) {
		this.namers = namers;
	}

	public void addNamer(Namer namer) {
		this.namers.add(namer);
	}
}
