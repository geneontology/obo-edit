package org.geneontology.gold.io;

/**
 * This interface is a listener for file monitoring process. The filesModified method is fired when a file or files are modified which
 * are moniored for updates by {@link FileMonitor}.
 * @author Shahid Manzoor
 *
 */
public interface FileMonitorListener {
	public void filesModified(String files[]);
}
