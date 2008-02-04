package org.bbop.framework;

import javax.swing.JOptionPane;

public class CheckMemoryThread extends Thread {

  // -----------------------------------------------------------------------
  // Instance variables
  // -----------------------------------------------------------------------

  long minMemory;
  long maxMemory;
  long interval = 30*1000; // in milliseconds--i.e., 30 seconds
  private boolean halt = false;

  public CheckMemoryThread() {
    maxMemory = Runtime.getRuntime().maxMemory();
    // Complain if free memory goes below 5% of max memory
    minMemory = maxMemory/(long)20;
    setDaemon(true);
    System.err.println("CheckMemoryThread: max heap size = " + maxMemory + "; warn if available memory < " + minMemory);
  }

  public void checkFreeMemory() {
    // Using freeMemory() is not a good way to check free memory, because the heap might
    // still be at its initial smaller size.  We might have almost used up that initial
    // heap, but then the JVM can allocate more memory, up to the max.
    //    long freeMemory = Runtime.getRuntime().freeMemory();

    // It seems like we're able to allocate more than maxMemory memory.
    // Not sure how much more.  But it's possible to get freeMemory values
    // below 0!
    long memoryUsed = Runtime.getRuntime().totalMemory();
    long freeMemory = maxMemory - memoryUsed;
    System.err.println("checkFreeMemory: free memory = " + freeMemory + ", total memory used = " + memoryUsed);

    if (freeMemory < minMemory) {
      // Try garbage collecting first and see if that helps.
      try {
	System.gc();
	sleep(2000);
	// Do it twice because it seems to get more the second time.
	System.gc();
	sleep(2000);
      } catch (InterruptedException e) {}

      memoryUsed = Runtime.getRuntime().totalMemory();
      freeMemory = maxMemory - memoryUsed;
      System.err.println("checkFreeMemory: After garbage collecting, free memory = " + freeMemory);
      if (freeMemory < minMemory) {
	String m = "WARNING: you are almost out of memory (" + freeMemory + " bytes left).\nIf you run out of memory, this application could crash and you could lose your work.\nWe recommend saving now, then exiting the application and restarting.";
	System.err.println(m);
	JOptionPane.showMessageDialog(null,m);
	System.err.println("checkFreeMemory: free memory = " + freeMemory + ", total memory used = " + memoryUsed);
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
