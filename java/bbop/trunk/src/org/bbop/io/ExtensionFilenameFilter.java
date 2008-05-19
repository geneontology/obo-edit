package org.bbop.io;

import java.io.*;

import org.apache.log4j.*;

public class ExtensionFilenameFilter implements FilenameFilter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExtensionFilenameFilter.class);

    protected String [] extensions;
    protected String desc;
    protected boolean showHidden;

    private static String [] createExtensionArray(String ext) {
	String [] exts = new String[1];
	exts[0] = ext;
	return exts;
    }

    public ExtensionFilenameFilter(String extension) {
	this(extension, null);
    }

    public ExtensionFilenameFilter(String extension, String desc) {
	this(createExtensionArray(extension), desc);
    }

    public ExtensionFilenameFilter(String [] extensions) {
	this(extensions, null);
    }

    public ExtensionFilenameFilter(String [] extensions, String desc) {
	this.extensions = extensions;
	this.desc = desc;
	this.showHidden = false;
    }

    public boolean accept(File dir, String file) {
	File path = new File(dir, file);
	if (!showHidden && file.startsWith("."))
	    return false;
	if (path.isDirectory())
	    return true;
	for(int i=0; i < extensions.length; i++) {
	    if (file.endsWith("."+extensions[i]))
		return true;
	}
	return false;
    }

    public void setShowHidden(boolean showHidden) {
	this.showHidden = showHidden;
    }

    public boolean getShowHidden() {
	return showHidden;
    }

    protected String getExtensionDesc() {
	StringBuffer out = new StringBuffer();
	for(int i=0; i < extensions.length; i++) {
	    if (i > 0)
		out.append(",");
	    out.append("*."+extensions[i]);
	}
	return out.toString();
    }

    public String toString() {
	if (desc == null)
	    return getExtensionDesc();
	else
	    return desc+" ("+getExtensionDesc()+")";
    }
}
