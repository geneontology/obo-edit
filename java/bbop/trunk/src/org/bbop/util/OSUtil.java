package org.bbop.util;

import java.io.File;

/** This class provides utility methods for determining operating system
 *  specific information.
 *
 *  @author elee
 */

import org.apache.log4j.*;

public class OSUtil
	{

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OSUtil.class);

  /** Enum representing values for supported operating systems
   */
  public enum OS
  {
	  // !! TODO Need to add Windows 8, when it becomes available
    LINUX,
    MAC_OS_X,
    WINDOWS_7,
    WINDOWS_VISTA,
    WINDOWS_XP,
    WINDOWS_2000,
    WINDOWS_98,
    WINDOWS_ME,
    SUN,
    UNKNOWN
  }

  /** Convenience method for retrieving os.name system property
   *
   *  @return os.name system property
   */
  public static String getOSName()
  {
    return System.getProperty("os.name");
  }

  /** Convenience method for retrieving os.version system property
   *
   *  @return os.version system property
   */
  public static String getOSVersion()
  {
    return System.getProperty("os.version");
  }

  /** Convenience method for retrieving os.arch system property
   *
   *  @return os.arch system property
   */
  public static String getOSArch()
  {
    return System.getProperty("os.arch");
  }

  /** Get the operating system the application is running on
   *
   *  @return the OS the application is running on
   */
  public static OS getOS()
  {
    String osName = getOSName();
    String osVersion = getOSVersion();

    //Linux
    if (osName.equalsIgnoreCase("Linux")) {
      return OS.LINUX;
    }
    //Mac OS X
    else if (osName.equalsIgnoreCase("Mac OS X")) {
      return OS.MAC_OS_X;
    }
    //Windows (this is the trouble case, as there are many different
    //flavors of Windows...In case that 'osName' doesn't return the full
    //name of Windows, need to fall back on osVersion
    else if (osName.matches("^Windows.*$")) {
    	 //Windows Vista
      if (osName.equalsIgnoreCase("Windows 7") ||
            osVersion.equals("6.1")) {
         return OS.WINDOWS_7;
      }
      //Windows Vista
      else if (osName.equalsIgnoreCase("Windows Vista") ||
          osVersion.equals("6.0")) {
        return OS.WINDOWS_VISTA;
      }
      //Windows XP
      else if (osName.equalsIgnoreCase("Windows XP") ||
              osVersion.equals("5.1")) {
        return OS.WINDOWS_XP;
      }
      //Windows 2000
      else if (osName.equalsIgnoreCase("Windows 2000") ||
               osVersion.equals("5.0")) {
        return OS.WINDOWS_2000;
      }
      //Windows 98
      else if (osName.equalsIgnoreCase("Windows 98") ||
               osVersion.startsWith("4.10")) {
        return OS.WINDOWS_98;
      }
      //Windows ME
      else if (osName.equalsIgnoreCase("Windows ME") ||
               osVersion.equals("4.10")) {
        return OS.WINDOWS_ME;
      }
      //Linux
      else if (osName.equalsIgnoreCase("SunOS")) {
	      return OS.SUN;
    }
    }
    return OS.UNKNOWN;
  }

  /** Check to see if the OS is Linux.
   *
   *  @return true if the OS is Linux
   */
  public static boolean isLinux()
  {
    return getOS() == OS.LINUX;
  }

  /** Check to see if the OS is SunOS
   *
   *  @return true if the OS is SunOS
   */
  public static boolean isSun()
  {
    return getOS() == OS.SUN;
  }

  /** Check to see if the OS is Mac OS X.
   *
   *  @return true if the OS is Mac OS X
   */
  public static boolean isMacOSX()
  {
    return getOS() == OS.MAC_OS_X;
  }

  /** Check to see if the OS is Windows 7
   *
   *  @return true if the OS is Windows 7
   */
  public static boolean isWindows7()
  {
    return getOS() == OS.WINDOWS_7;
  }
  
  /** Check to see if the OS is Windows Vista
   *
   *  @return true if the OS is Windows Vista
   */
  public static boolean isWindowsVista()
  {
    return getOS() == OS.WINDOWS_VISTA;
  }

  /** Check to see if the OS is Windows XP
   *
   *  @return true if the OS is Windows XP
   */
  public static boolean isWindowsXP()
  {
    return getOS() == OS.WINDOWS_XP;
  }

  /** Check to see if the OS is Windows 2000
   *
   *  @return true if the OS is Windows 2000
   */
  public static boolean isWindows2000()
  {
    return getOS() == OS.WINDOWS_2000;
  }

  /** Check to see if the OS is Windows 98
   *
   *  @return true if the OS is Windows 98
   */
  public static boolean isWindows98()
  {
    return getOS() == OS.WINDOWS_98;
  }

  /** Check to see if the OS is Windows ME
   *
   *  @return true if the OS is Windows ME
   */
  public static boolean isWindowsME()
  {
    return getOS() == OS.WINDOWS_ME;
  }

  /** Check to see if the OS is any flavor of Windows
   *
   *  @return true if the OS is any flavor of Windows
   */
  public static boolean isWindows()
  {
    if (isWindows7() || isWindowsVista() || 
        isWindowsXP() || isWindows2000() ||
        isWindows98() || isWindowsME()) {
      return true;
    }
    return false;
  }

  /** Check to see if the OS is any currently recognized flavor of Unix (Linux or SunOS)
   *
   *  @return true if the OS is any currently recognized flavor of Unix (Linux or SunOS)
   */
  public static boolean isUnix()
  {
	  if (isLinux() || isSun())
		  return true;
	  return false;
  }

  /** Get the suggested configuration directory given the operating
   *  system.  Follows the convention used by Firefox.
   *
   *  @param projectName - name of the project
   *  @return suggested configuration directory location.  Returns null
   *          if the suggested configuration directory cannot be determined
   */
  public static String getConfigDirectory(String projectName)
  {
    String userHome = System.getProperty("user.home");
    if (isUnix()) {
	    return userHome + "/." + projectName.toLowerCase();
    }
    else if (isMacOSX()) {
      return userHome + "/Library/Application Support/" + projectName;
    }
    else if (isWindowsVista()) {
      return userHome + "\\AppData\\Roaming\\" + projectName;
    }
    else if (isWindowsXP() || isWindows2000()) {
      return userHome + "\\Application Data\\" + projectName;
    }
    else if (isWindows98() || isWindowsME()) {
      return File.listRoots()[0] + "\\Windows\\Application Data\\" +
             projectName;
    }
    return null;
  }

}
