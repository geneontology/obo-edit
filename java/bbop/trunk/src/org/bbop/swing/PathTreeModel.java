package org.bbop.swing;

import java.util.Vector;

import javax.swing.tree.TreePath;

import org.apache.log4j.Logger;

public class PathTreeModel extends SimpleTreeModel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PathTreeModel.class);

	protected Vector<TreePath> paths = new Vector<TreePath>();

	public PathTreeModel() {
	}

	public PathTreeModel(TreePath [] paths) {	
		setPaths(paths);
	}

	public Vector<TreePath> getPaths() {
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

	public void setPaths(Vector<TreePath> paths) {
		parentage.clear();
		this.paths.removeAllElements();
		for(int i=0; i < paths.size(); i++)
			addPath(paths.elementAt(i));
	}
}






