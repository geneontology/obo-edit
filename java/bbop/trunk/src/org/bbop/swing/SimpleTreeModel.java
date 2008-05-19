package org.bbop.swing;

import java.util.*;
import javax.swing.tree.*;

import org.apache.log4j.*;

public class SimpleTreeModel extends AbstractTreeModel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleTreeModel.class);
    protected Hashtable parentage = new Hashtable();
    protected Object root = null;

    public void setRoot(Object root) {
	this.root = root;
    }

    public void addChild(Object parent, Object child) {
	Vector v = (Vector) parentage.get(parent);
	if (v == null) {
	    v = new Vector();
	    parentage.put(parent, v);
	}
	if (!v.contains(child))
	    v.addElement(child);
    }

    public void removeChild(Object parent, Object child) {
	Vector v = (Vector) parentage.get(parent);
	if (v == null) {
	    v = new Vector();
	    parentage.put(parent, v);
	}
	v.removeElement(child);
    }

    public Object getChild(Object parent, int index) {
	Vector v = (Vector) parentage.get(parent);
	if (v == null || index >= v.size())
	    return null;
	return v.elementAt(index);
    }

    public int getChildCount(Object parent) {
	Vector v = (Vector) parentage.get(parent);
	if (v == null)
	    return 0;
	else
	    return v.size();
    }

    public int getIndexOfChild(Object parent, Object child) {
	Vector v = (Vector) parentage.get(parent);
	if (v == null)
	    return -1;
	else
	    return v.indexOf(child);
    }

    protected Enumeration getAllLeaves() {
	Enumeration e = parentage.keys();
	Vector leaves = new Vector();
	while(e.hasMoreElements()) {
	    Object node = e.nextElement();
	    Vector v = (Vector) parentage.get(node);
	    for(int i=0; i < v.size(); i++) {
		Object child = v.elementAt(i);
		Vector childv = (Vector) parentage.get(child);
		if (childv == null || childv.size() == 0)
		    leaves.addElement(child);
	    }
	}
	return leaves.elements();
    }

    public Object getRoot() {
	return root;
    }

    public boolean isLeaf(Object o) {
	Vector v = (Vector) parentage.get(o);
	return v == null || v.size() == 0;
    }

    public void valueForPathChanged(TreePath path, Object newValue) {}

    public String toString() {
	StringBuffer buffer = new StringBuffer();
	Enumeration e = parentage.keys();
	while(e.hasMoreElements()) {
	    Object o = e.nextElement();
	    buffer.append(o+" = "+parentage.get(o)+"\n");
	}
	return buffer.toString();
    }
}






