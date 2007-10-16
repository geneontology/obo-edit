package org.bbop.util;

import java.util.*;

public class MultiProperties extends Properties {
    /**
	 * 
	 */
	private static final long serialVersionUID = 445024832917738700L;

	private static final String DELIM = "%";

    private MultiProperties parent = null;
    private String myKey = "";

    public MultiProperties getProperties(String key) {
	MultiProperties out = new MultiProperties();
	out.parent = this;
	out.myKey = key;
	Enumeration e = propertyNames();
	while(e.hasMoreElements()) {
	    String elt = (String) e.nextElement();
	    String header = getKeyHead(elt);
	    String tail = getKeyTail(elt);
	    // Have to test for null, as we are getting a null pointer exception
	    if (header!=null && header.equals(key))
		out.put(tail, getProperty(elt));
	}
	return out;
    }

    public Object setProperty(String key, String value) {
	Object out = super.put(key, value);
	if (parent != null) {
	    parent.setProperty(myKey+DELIM+key, value);
	}
	return out;
    }

    public void setProperties(String key, Properties p) {
	removeProperties(key);
	addProperties(key, p);
    }

    public void addProperties(String key, Properties p) {
	Enumeration e = p.propertyNames();
	while(e.hasMoreElements()) {
	    String elt = (String) e.nextElement();
	    setProperty(key+DELIM+elt, p.getProperty(elt));
	}
    }

    public void removeProperties(String key) {
	Properties props = getProperties(key);
	Enumeration e = props.propertyNames();
	while(e.hasMoreElements()) {
	    String elt = (String) e.nextElement();
	    remove(elt);
	}	
    }

    public Enumeration multiPropertyNames() {
	Hashtable out = new Hashtable();
	Enumeration e = propertyNames();
	while(e.hasMoreElements()) {
	    String elt = (String) e.nextElement();
	    StringTokenizer st = new StringTokenizer(elt, DELIM);
	    if (st.hasMoreTokens())
		out.put(elt, elt);
	}
	return out.elements();
    }

    protected String getKeyHead(String key) {
	StringTokenizer tokenizer = new StringTokenizer(key, DELIM);
	try {
	    return tokenizer.nextToken();
	} catch (Exception e) {
	    return null;
	}
    }

    protected String getKeyTail(String key) {
	StringTokenizer tokenizer = new StringTokenizer(key, DELIM);
	try {
	    tokenizer.nextToken();
	    StringBuffer out = new StringBuffer();
	    while(tokenizer.hasMoreTokens()) {
		if (out.length() != 0)
		    out.append(DELIM);
		out.append(tokenizer.nextToken());
	    }
	    return out.toString();
	} catch (Exception e) {
	    return null;
	}
    }

}
