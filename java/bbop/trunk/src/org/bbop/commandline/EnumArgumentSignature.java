package org.bbop.commandline;

import java.util.*;

import org.apache.log4j.*;

public class EnumArgumentSignature implements ArgumentSignature {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EnumArgumentSignature.class);

    protected List signatures = new ArrayList();
    protected ArgumentSignature current;
    protected int id = CommandLineParser.getID();
    protected int index = 0;
    protected boolean triedDefault = false;
    int defaultIndex = -1;
    protected String shortDocumentation;

    public int getID() {
	return id;
    }

    @Override
	public boolean equals(Object o) {
	if (o instanceof ArgumentSignature)
	    return ((ArgumentSignature) o).getID() == id;
	else
	    return false;
    }

    @Override
	public int hashCode() {
	return id;
    }

    public EnumArgumentSignature() {
	this(null);
    }

    public EnumArgumentSignature(String shortDocumenation) {
	this.shortDocumentation = shortDocumenation;
    }

    public String getShortDocumentation() {
	if (shortDocumentation == null) {
	    StringBuffer out = new StringBuffer();
	    Iterator it = signatures.iterator();
	    boolean first = true;
	    while(it.hasNext()) {
		ArgumentSignature as = (ArgumentSignature) it.next();
		if (!first)
		    out.append(" | ");
		out.append(as.getShortDocumentation());
		first = false;
	    }
	    return "("+out.toString()+")";
	} else
	    return shortDocumentation;
    }

    @Override
	public String toString() {
	return super.toString();
    }

    public void setOnlyAcceptAsLastResort(boolean lastResort) {}

    public ArgumentSignature copy() {
	EnumArgumentSignature out = new EnumArgumentSignature();
	Iterator it = signatures.iterator();
	while(it.hasNext()) {
	    ArgumentSignature sig = (ArgumentSignature) it.next();
	    out.signatures.add(sig.copy());
	}
	if (current != null)
	    out.current = current.copy();
	out.id = id;
	out.defaultIndex = defaultIndex;
	out.index = index;
	out.triedDefault = triedDefault;
	return out;
    }

    public void init(CommandLineParser p, boolean defaultMode) {
	current = null;
	index = 0;
	triedDefault = false;
	Iterator it = signatures.iterator();
	while(it.hasNext()) {
	    ArgumentSignature sig = (ArgumentSignature) it.next();
	    sig.init(p, false);
	}
    }

    public boolean onlyAcceptAsLastResort() {
	return false;
    }

    public void addSignature(ArgumentSignature as) {
	addSignature(as, false);
    }

    public void addSignature(ArgumentSignature as, boolean isDefault) {
	as.setOnlyAcceptAsLastResort(false);
	signatures.add(as);
	if (isDefault)
	    defaultIndex = signatures.size() - 1;
    }

    public void accept(CommandLineParser p) throws FailException {
	if (current != null)
	    throw new FailException("UNEXPECTED CONDITION: Illegal "+
				    "attempt to reassign enum value");
	for( ; index < signatures.size(); index++) {	    
	    ArgumentSignature sig = (ArgumentSignature) signatures.get(index);
	    try {
		p.doAccept(sig);
		current = sig.copy();
		return;
	    } catch (FailException ex) {
		// just try the next guy
	    }
	}
	if (!triedDefault && defaultIndex != -1) {
	    ArgumentSignature sig = (ArgumentSignature) signatures.
		get(defaultIndex);
	    sig.init(p, true);
	    p.doAccept(sig, true);
	    current = sig.copy();
	    return;
	}
	String optionStr = signatures.toString();
	throw new FailException("Expected "+optionStr+", found "+
				p.peekNextString());
    }

    public List getValues() throws UnfullfilledException {
	if (current == null) {
	    throw new UnfullfilledException();
	}
	return current.getValues();
    }
}
