package org.bbop.swing;

import javax.swing.tree.*;
import java.io.*;
import java.util.*;

import org.bbop.util.*;

public class FileSystemTreeModel extends AbstractTreeModel {

    public class DummyRoot {
	protected String name;

	public DummyRoot(String name) {
	    this.name = name;
	}

	public String toString() {
	    return name;
	}
    }

    protected static Comparator defaultComparator = new Comparator() {
	    public int compare(Object obja, Object objb) {
		File a = (File) obja;
		File b = (File) objb;
		if (a.isDirectory() && !b.isDirectory())
		    return -1;
		if (b.isDirectory() && !a.isDirectory())
		    return 1;
		if (b.isHidden() && !a.isHidden())
		    return 1;
		int compVal = a.getName().compareToIgnoreCase(b.getName());
		if (compVal < 0)
		    return -1;
		else if (compVal > 0)
		    return 1;
		else
		    return 0;
	    }
	};

    protected FilenameFilter showFoldersWrapperFilter = new FilenameFilter() {
	    public boolean accept(File dir, String name) {
		File file = new File(dir, name);
		if (file.isDirectory())
		    return true;
		else
		    return filter.accept(dir, name);
	    }
	};

    protected FilenameFilter defaultFilter = new FilenameFilter() {
	    public boolean accept(File dir, String name) {
		return true;
	    }
	};

    protected VectorFilter hiddenFilter = new VectorFilter() {
	    /**
		 * 
		 */
		private static final long serialVersionUID = -627211135110757782L;

		public boolean satisfies(Object o) {
		return !((File) o).isHidden() &&
		    !((File) o).getName().startsWith(".");
	    }
	};

    protected Object root;
    protected HashMap childCache = new HashMap();
    protected boolean showHidden = false;
    protected FilenameFilter filter;

    public FileSystemTreeModel() {
	this(null, null);
    }

    public FileSystemTreeModel(FilenameFilter filter) {
	this(null, filter);
    }

    public FileSystemTreeModel(File root) {
	this(root, null);
    }

    public FileSystemTreeModel(File root, FilenameFilter filter) {
	setRoot(root, false);
	setFilter(filter, false);
	reload();
    }

    public void setRoot(Object root) {
	setRoot(root, true);
    }

    protected void setRoot(Object root, boolean reload) {
	if (root == null)
	    this.root = getDefaultRoot();
	else
	    this.root = root;
	if (reload)
	    reload();
    }

    public FilenameFilter getFilter() {
	return filter;
    }

    public void setFilter(FilenameFilter filter) {
	setFilter(filter, true);
    }

    protected void setFilter(FilenameFilter filter, boolean reload) {
	if (filter == null)
	    this.filter = getDefaultFilter();
	else
	    this.filter = filter;
	if (reload)
	    reload();
    }

    protected Comparator getDefaultComparator() {
	return defaultComparator;
    }

    protected FilenameFilter getDefaultFilter() {
	return defaultFilter;
    }

    public boolean getShowHidden() {
	return showHidden;
    }

    public void setShowHidden(boolean showHidden) {
	this.showHidden = showHidden;
    }

    public Object getDefaultRoot() {
	File [] fileRoots = File.listRoots();
	if (fileRoots.length == 1)
	    return fileRoots[0];
	else
	    return new DummyRoot("My computer");
    }

    protected Vector getChildren(Object parent) {
	Vector out = (Vector) childCache.get(parent);
	if (out == null) {
	    out = new Vector();
	    File [] files = null;
	    if (parent instanceof DummyRoot) {
		files = File.listRoots();
	    }
	    File parentFile = (File) parent;
	    if (parentFile.isDirectory()) {
		files = parentFile.listFiles(showFoldersWrapperFilter);
	    }
	    if (files != null) {
		for(int i=0; i < files.length; i++)
		    out.add(files[i]);
		if (!showHidden)
		    out = VectorUtil.filter(hiddenFilter, out);
		Collections.sort(out, getDefaultComparator());
	    }
	    childCache.put(parent, out);
	}
	    
	return out;
    }

    public void reload() {
	childCache.clear();
	super.reload();
    }

    public TreePath getTreePath(File file) {
	Vector out = new Vector();
	do {
	    out.insertElementAt(file, 0);
	    file = file.getParentFile();
	} while(file != null && !file.equals(root));

	if ((file == null && root instanceof DummyRoot) ||
	    (file != null && file.equals(root)))
	    out.insertElementAt(root, 0);

	Object [] nodes = new Object[out.size()];
	for(int i=0; i < nodes.length; i++)
	    nodes[i] = out.elementAt(i);
	return new TreePath(nodes);
    }

    public Object getRoot() {
	return root;
    }

    public Object getChild(Object parent, int index) {
	return getChildren(parent).elementAt(index);
    }

    public int getChildCount(Object parent) {
	return getChildren(parent).size();
    }

    public boolean isLeaf(Object node) {
	return (getChildCount(node) == 0);
    }

    public int getIndexOfChild(Object parent, Object child) {
	return getChildren(parent).indexOf(child);
    }

    public void valueForPathChanged(TreePath path, Object o) {}
}
