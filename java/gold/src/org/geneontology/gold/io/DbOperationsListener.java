package org.geneontology.gold.io;

/**
 * This class provides listener methods to be called by DbOperations 
 * during the start and end of the each data base operations
 * @author Shahid Manzoor
 *
 */
public interface DbOperationsListener {

	/**
	 * This method is called when DbOperations.bulkLoad method is called.
	 */
	public void bulkLoadStart();
	
	/**
	 * This method is called when DbOperations.bulkLoad method completes is operation
	 */
	public void bulkLoadEnd();
	
	/**
	 * This method is called when DbOperations.dumpFiles method is called.
	 */
	public void dumpFilesStart();

	/**
	 * This method is called when DbOperations.dumpFiles method completes is operation
	 */
	public void dumpFilesEnd();
	
	
	/**
	 * This method is called when DbOperations.buildSchema method is called.
	 */
	public void buildSchemaStart();
	
	/**
	 * This method is called when DbOperations.buildSchema method completes is operation
	 */	
	public void buildSchemaEnd();
	
	/**
	 * This method is called when DbOperations.loadTsvFiles method is called.
	 */	
	public void loadTsvFilesStart();
	
	/**
	 * This method is called when DbOperations.loadTsvFiles method completes is operation
	 */
	public void loadTsvFilesEnd();
	
	/**
	 * This method is called when DbOperations.update method is called.
	 */	
	public void updateStart();
	
	/**
	 * This method is called when DbOperations.update method completes is operation
	 */
	public void updateEnd();
	
}
