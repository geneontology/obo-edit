package org.geneontology.gold.io;

/**
 * Covers all objects that handle database loading
 * 
 * @author cjm
 *
 */
public interface Loader {
	
	/**
	 * @return true if source has data that is more recent than target db
	 */
	public boolean isLoadRequired();
	
	/**
	 * load data from source to target db
	 */
	public void load();
	
	public void setSource(String src);

}
