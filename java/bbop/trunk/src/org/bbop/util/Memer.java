/*
**  File: Memer.java
**
*/

package org.bbop.util;

/**
 * A little utility class to help monitor memory usage.
 */
import org.apache.log4j.*;

public class Memer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Memer.class);

  Runtime rt;
  long prevCheck, currCheck;
  long currFreeMem, currTotalMem, currMemUsed, deltaMemUsed, 
       prevFreeMem, prevTotalMem, prevMemUsed;
 
  public Memer() {
    rt = Runtime.getRuntime();
    currFreeMem = currTotalMem = currMemUsed = deltaMemUsed = 0;
    prevFreeMem = prevTotalMem = prevMemUsed = 0;
    calcMem();
  }

  /**
   * checks memory
   * and prints a message if the total memory usage has changed
   * since the last time this was called.
   */
  public void checkMemory() {
    currCheck = rt.totalMemory();
    if (prevCheck != currCheck) {
      printMemory();
      prevCheck = currCheck;
    }
  }

  /**
   * refreshes this object's data from the Runtime.
   */
  public void calcMem() {
    currFreeMem = rt.freeMemory();    
    currTotalMem = rt.totalMemory();
    currMemUsed = currTotalMem - currFreeMem;
    deltaMemUsed = currMemUsed - prevMemUsed;
    prevFreeMem = currFreeMem;
    prevTotalMem = currTotalMem;
    prevMemUsed = currMemUsed;
  }

  /**
   * returns the change in memory usage
   * between the last two calls to calcMem.
   *
   * @return the most recent change in memory usage.
   * @see #calcMem
   */
  public long getLastDelta() {
    return deltaMemUsed;
  }

  /**
   * prints this object's data to stdout.
   */
  public void printMemory() {
    System.out.println(this.toString());
  }

  /**
   * @return a string representation of this object.
   */
  @Override
public String toString() {
    calcMem();
    return " free: " + currFreeMem + 
           "   total: " + currTotalMem + 
           "   used: " + currMemUsed +
           "   delta: " + deltaMemUsed;
  }

}
