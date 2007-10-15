package org.bbop.commandline;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class FlagSpec extends TagSpec {
	protected FlagSpec() {
		
	}
	
	public FlagSpec(String name) {
		super(name, new OrderedArgumentSignature());
	}
	
	public ArgumentSignature copy() {
		FlagSpec out = new FlagSpec();
		out.primaryName = primaryName;
		out.signature = signature.copy();
		out.nameSig = (EnumArgumentSignature) nameSig.copy();
		return out;
	}
}
