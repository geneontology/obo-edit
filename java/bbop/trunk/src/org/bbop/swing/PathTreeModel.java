package org.bbop.swing;

import javax.swing.tree.*;
import java.util.*;

public class PathTreeModel extends SimpleTreeModel {

    protected Vector paths = new Vector();

    public PathTreeModel() {
    }

    public PathTreeModel(TreePath [] paths) {	
	setPaths(paths);
    }

    public Vector getPaths() {
	return paths;
    }

    public void removePath(TreePath path) {
	paths.removeElement(path);
	Object parent = path.getLastPathComponent();
	Object child = path.getPathComponent(path.getPathCount()-2);
	removeChild(parent, child);
    }

    public void addPath(TreePath path) {
	paths.addElement(path);
	Object [] obs = path.getPath();
	for(int i=0; i < obs.length; i++) {
	    if (i == 0)
		setRoot(obs[0]);
	    else
		addChild(obs[i-1], obs[i]);
	}
    }

    public void setPaths(TreePath [] paths) {
	parentage.clear();
	this.paths.removeAllElements();
	for(int i=0; i < paths.length; i++)
	    addPath(paths[i]);
    }

    public void setPaths(Vector paths) {
	parentage.clear();
	this.paths.removeAllElements();
	for(int i=0; i < paths.size(); i++)
	    addPath((TreePath) paths.elementAt(i));
    }
}






