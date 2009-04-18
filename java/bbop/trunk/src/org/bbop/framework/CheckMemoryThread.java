package org.bbop.framework;

import javax.swing.JOptionPane;
import org.apache.log4j.*;

import org.apache.log4j.*;

public class CheckMemoryThread extends Thread {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CheckMemoryThread.class);
	

  // -----------------------------------------------------------------------
  // Instance variables
  // -----------------------------------------------------------------------

  long minMemory;
  long maxMemory;
  long interval = 40*1000; // in milliseconds--i.e., 40 seconds
  private boolean halt = false;

  public CheckMemoryThread() {
    maxMemory = Runtime.getRuntime().maxMemory();
    // Complain if free memory goes below 10% of max memory
    minMemory = maxMemory/(long)10;
    setDaemon(true);
    //1 Megabyte = 1048576 Bytes
    //changing the info statement to MB to make it more readable
    logger.info("CheckMemoryThread: max heap size = " + maxMemory/1048576 + " MB; warn if available memory < " + minMemory/1048576 + " MB");
  }

  public void checkFreeMemory() {
    // It seems like we're able to allocate more than maxMemory memory.
    // Not sure how much more.  But it's possible to get freeMemory values
    // below 0!
//      System.gc();  // DEL
    long memoryUsed = Runtime.getRuntime().totalMemory();
    long freeMemory = maxMemory - memoryUsed;
//    logger.info("checkFreeMemory: free memory = " + freeMemory/1048576 + " MB, total memory used = " + memoryUsed/1048576 + " MB");

    if (freeMemory < minMemory) {
      logger.info("checkFreeMemory: free memory = " + freeMemory/1048576 + "MB, total memory used = " + memoryUsed/1048576 + "MB.  Garbage collecting.");
      // Try garbage collecting first and see if that helps.
      try {
	System.gc();
	sleep(2000);
	// calling garbage collection twice because it seems to get more the second time.
	System.gc();
	sleep(2000);
      } catch (InterruptedException e) {}

      memoryUsed = Runtime.getRuntime().totalMemory();
      freeMemory = maxMemory - memoryUsed;
//      logger.info("checkFreeMemory: After garbage collecting, free memory = " + freeMemory);
      if (freeMemory < minMemory) {
	String m = "WARNING: you are almost out of memory (" + freeMemory/1048576 + " MB left).\nIf you run out of memory, this application could crash and you could lose your work.\nWe recommend saving now, then exiting the application and restarting.";
	logger.info(m);
	JOptionPane.showMessageDialog(null,m);
	logger.info("checkFreeMemory: free memory = " + freeMemory/1048576 + "MB, total memory used = " + memoryUsed/1048576 + "MB");
	// We've warned once--don't warn again
	this.halt();
      }
    }
  }

  public void halt() {
    halt = true;
    interrupt();
  }

  public void run() {
    while(!halt) {
      try {
        sleep(interval);
	checkFreeMemory();
      } catch (InterruptedException e) {}
    }
  }
}
