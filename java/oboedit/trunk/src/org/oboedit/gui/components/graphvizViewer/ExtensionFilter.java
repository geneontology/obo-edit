package org.oboedit.gui.components.graphvizViewer;

import java.io.File;

public class ExtensionFilter extends javax.swing.filechooser.FileFilter {
	protected String ext;
	protected String desc;

	public ExtensionFilter(String ext, String desc) {
		this.ext = ext;
		this.desc = desc;
	}

	public boolean accept(File f) {
		return f.getName().endsWith(ext);
	}

	public String getDescription() {
		return desc;
	}

	public String getExt() {
		return ext;
	}

	public String getExtNoDot() {
		return ext.substring(1, ext.length());
	}
}

