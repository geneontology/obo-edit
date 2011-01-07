package owltools.sim;

import org.semanticweb.owlapi.model.OWLObject;

public class OWLObjectPair {
	private final OWLObject a;
	private final OWLObject b;
	private transient final int hash;

	public OWLObjectPair(OWLObject a, OWLObject b) {
		super();
		this.a = a;
		this.b = b;
		hash = (a == null? 0 : a.hashCode() * 31)+(b == null? 0 : b.hashCode());
	}
	@Override
	public int hashCode()
	{
		return hash;
	}
	public boolean equals(Object x) {
		if (!(x instanceof OWLObjectPair))
			return false;
		return ((OWLObjectPair)x).getA().equals(a) &&
		((OWLObjectPair)x).getA().equals(a);

	}
	public OWLObject getA() {
		return a;
	}
	public OWLObject getB() {
		return b;
	} 

	public String toString() {
		return "PAIR:{"+a.toString()+","+b.toString()+"}";
	}

}
