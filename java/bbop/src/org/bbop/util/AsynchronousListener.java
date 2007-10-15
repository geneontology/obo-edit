package org.bbop.util;

import java.util.EventListener;

/**
 * Used to indicate that a listener can be fired from any thread. This is most important
 * in Swing/AWT apps where listeners need to know their execution thread
 * @author jrichter
 *
 */
public interface AsynchronousListener extends EventListener {

}
