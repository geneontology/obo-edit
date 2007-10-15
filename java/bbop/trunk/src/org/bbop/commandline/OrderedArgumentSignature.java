package org.bbop.commandline;

import java.util.*;

public class OrderedArgumentSignature implements ArgumentSignature {

    protected List signatures = new ArrayList();
    int index = 0;
    protected int id = CommandLineParser.getID();
    protected String shortDocumentation;

    public int getID() {
	return id;
    }

    public boolean equals(Object o) {
	if (o instanceof ArgumentSignature)
	    return ((ArgumentSignature) o).getID() == id;
	else
	    return false;
    }

    public int hashCode() {
	return id;
    }

    public OrderedArgumentSignature() {
	this(null);
    }

    public OrderedArgumentSignature(String shortDocumentation) {
	this.shortDocumentation = shortDocumentation;
    }

    public String getShortDocumentation() {
	if (shortDocumentation == null) {
	    StringBuffer out = new StringBuffer();
	    Iterator it = signatures.iterator();
	    boolean first = true;
	    while(it.hasNext()) {
		ArgumentSignature sig = (ArgumentSignature) it.next();
		if (!first)
		    out.append(" ");
		out.append(sig.getShortDocumentation());
		first = false;
	    }
	    return out.toString();
	} else
	    return "<"+shortDocumentation+">";
    }

    protected OrderedArgumentSignature createCopyObject() {
	return new OrderedArgumentSignature();
    }

    public ArgumentSignature copy() {
	OrderedArgumentSignature out = createCopyObject();
	Iterator it = signatures.iterator();
	while(it.hasNext()) {
	    ArgumentSignature sig = (ArgumentSignature) it.next();
	    out.signatures.add(sig.copy());
	}
	out.id = id;
	out.index = index;
	return out;
    }

    public void init(CommandLineParser p, boolean defaultMode) {
	if (defaultMode)
	    index = 1;
	else
	    index = 0;
	Iterator it = signatures.iterator();
	while(it.hasNext()) {
	    ArgumentSignature sig = (ArgumentSignature) it.next();
	    sig.init(p, false);
	}
    }

    public String toString() {
	return super.toString();
    }

    public boolean onlyAcceptAsLastResort() {
	return false;
    }

    public void addSignature(ArgumentSignature as) {
	as.setOnlyAcceptAsLastResort(false);
	signatures.add(as);
    }

    public void setOnlyAcceptAsLastResort(boolean lastResort) {}

    public void accept(CommandLineParser p) throws FailException {
	for( ; index < signatures.size(); index++) {
	    ArgumentSignature sig = (ArgumentSignature) signatures.get(index);
	    p.doAccept(sig);

	}
    }

    public List getValues() throws UnfullfilledException {
	if (index < signatures.size())
	    throw new UnfullfilledException();

	List out = new ArrayList();
	Iterator it = signatures.iterator();
	while(it.hasNext()) {
	    ArgumentSignature as = (ArgumentSignature) it.next();
	    out.addAll(as.getValues());
	}
	return out;
    }
}
