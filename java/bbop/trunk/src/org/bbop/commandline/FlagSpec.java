package org.bbop.commandline;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.*;

public class FlagSpec extends TagSpec {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FlagSpec.class);
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
