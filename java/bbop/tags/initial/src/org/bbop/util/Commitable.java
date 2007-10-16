package org.bbop.util;

/**
 * An interface for any object that allows commit/cancel behavior.
 */
public interface Commitable {

    /**
     * Commits the action
     */
    public void commit();

    /**
     * Cancels the action
     */
    public void cancel();
}
