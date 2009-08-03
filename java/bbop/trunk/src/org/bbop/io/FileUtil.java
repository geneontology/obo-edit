package org.bbop.io;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

public class FileUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FileUtil.class);

	  public static final String FILE_SEPARATOR = System.getProperty("file.separator");
	  public static final String DASH = "-";
	  public static final String UNDERSCORE = "_";
	  public static final String DOT = ".";

	  /**
     * Checks to see if a file exists. If it doesn't, the file is created
     * by copying the specified resource from the System class loader.
     * Based on code written by Matthew Pocock
     */
    public static void ensureExists(File file, String resource)
	throws IOException {
	ClassLoader loader = getSystemClassLoader();
	ensureExists(file, loader, resource);
    }

    /**
     * Checks to see if a file exists. If it doesn't, the file is created
     * by copying the specified resource from the specified class loader.
     * Based on code written by Matthew Pocock
     */
    public static void ensureExists(File file, ClassLoader loader, String resource)
      throws IOException {

      // if file exists nothgin to - return
      if (file.exists())
        return;

      // Get InputStream from resource
      if (loader == null) {
        throw new IOException("ClassLoader is null,"+
                              " can't load resource.");
      }
        
      if (resource == null) {
        throw new NullPointerException("Can't load from null resource");
      }
      
      InputStream is = loader.getResourceAsStream(resource);
      if (is == null) {
//        logger.debug("ensureExists: couldn't find resource "+resource+" gonna try "
//                           +"class.getResource");
        try { is = findInputStream(resource); }
        catch (FileNotFoundException e) {
//          logger.debug("ensureExists: couldn't find resource "+resource+" gonna try "
//                             +"/"+resource);
          is = findInputStream("/"+resource); //let ex fly
        }

        if (is == null)
          throw new FileNotFoundException("Couldn't find resource "+resource);
      }

      // Got InputStream from resource copy to file
      (new File(file.getParent())).mkdirs();
      OutputStream os = new BufferedOutputStream(
        new FileOutputStream(file));
      
//      logger.info("ensureExists: creating " + file.getAbsolutePath() + " from resource "
//                         + resource); // DEL
      
      logger.info("Creating " + file.getAbsolutePath() + " from resource "
              + resource);
      for(int next = is.read(); next != -1; next = is.read()) {
        os.write(next);
      }
      
      os.flush();
      os.close();
      //}
    }

  private static InputStream findInputStream(String resource)
    throws FileNotFoundException {
    URL u = FileUtil.class.getResource(resource);
    if (u == null) throw new FileNotFoundException("cant get resource "+resource);
    InputStream is=null;
    try { is = u.openStream(); }
    catch (IOException e) {}
    if (is == null) {throw new FileNotFoundException("failed to open stream "+resource);}
    return is;
  }

    public static ClassLoader getSystemClassLoader() {
	try {
	    Method loaderMethod = ClassLoader.class.getMethod(
						    "getSystemClassLoader",
						    new Class[0]);
	    Object out = loaderMethod.invoke(null, new Object[0]);
	    return (ClassLoader) out;
	} catch (Exception e) {
	    return Class.class.getClassLoader();
	}	
    }
    
    /** split into findMaster?ConfigUrl & findOboUrl!!! */
    public static URL findUrl(String filename) throws FileNotFoundException {
      if (filename == null) {
        String m = "cant find null file";
        logger.error(m); System.out.println(m);
        throw new FileNotFoundException(m);
      }
      List<URL> possibleUrls = getPossibleUrls(filename);
      for (URL u : possibleUrls) {  ///there's no particular order here, is there???
        //System.out.println(u+" url exists "+urlExists(u));
        if (urlExists(u)) return u;
      }
      //System.out.println("Failed to find file "+filename);
      //LOG.error("Failed to find file "+filename);
      throw new FileNotFoundException(filename+" not found");
    }

    // this is muddling config and obo - probably should be 2 methods? or be smart about
    // suffix - or who cares?
    // ok this is even sillier as it will look in .phenote for obo files and not for 
    // conf files - which is what the app wants but really there needs to be 2 different
    // methods - this is sheer laziness! also with separate methods can print out
    // better error message
    // split into getPossibleMaster?ConfigUrls & getPossibleOboUrls
    private static List<URL> getPossibleUrls(String filename) {
      List<URL> urls = new ArrayList<URL>(5);
      if (filename == null) {
        System.out.println("cant find null file");
        logger.error("cant find null file");
        return urls; // ?? ex?
      }
      // hmmm - should full path go last? can be problem with running from
      // jar as config files are in root(fix), obo files finally given dir
      addFile(filename,urls); // full path or relative to pwd
      addFile("conf/"+filename,urls);
      addFile("images/"+filename,urls);
      // ~/.phenote/obo-files cache for obo files - overrides phenote obo-files
      // eventually may have configured obo dir as well...
      addFile(getOboDir().getPath()+"/"+filename,urls);
      // addFile(getDotPhenoteConfDir().getPath() ???
      addFile("obo-files/"+filename,urls);//this is obo-files specific - separate method?
//       URL jarUrl = FileUtil.class.getResource(filename);
//       if (jarUrl != null) urls.add(jarUrl);
//       jarUrl = FileUtil.class.getResource("/"+filename);
//       if (jarUrl != null) urls.add(jarUrl);
      return urls;
    }

    // make an inner class for this?
    private static void addFile(String filename,List<URL> urls) {
      if (filename == null) {
        System.out.println("cant find null file");
        logger.error("cant find null file");
        return;
      }
      try {
        URL u = new File(filename).toURL();
        if (u != null) urls.add(u);

        URL jarUrl = FileUtil.class.getResource(filename);
        if (jarUrl != null) urls.add(jarUrl);
        jarUrl = FileUtil.class.getResource("/"+filename);
        if (jarUrl != null) urls.add(jarUrl);
      } catch (MalformedURLException e) {
        //System.out.println("bad file url "+e);
        logger.error("bad file url "+e);
      }
    }
    
    public static boolean urlExists(URL u) {
        try { u.openStream(); }
        catch (IOException e) { return false; }
        return true;
      }

    public static File getOboDir() {
        return getSubDir("obo-files");
      }

    private static File getSubDir(String subdirString) {
        File d = getDotHomeDir();
        File subdir = new File(d,subdirString);
        if (!subdir.exists()) {
          FileUtil.logger.info("creating "+subdir+" directory");
          subdir.mkdir();
        }
        return subdir;
      }
    
    /** if ~/.phenote doesnt exist yet its created */
    public static File getDotHomeDir() {
      String home = System.getProperty("user.home");
      String app_name = System.getProperty("application");
      File dotHome = new File(sep(home,"."+app_name));
      if (!dotHome.exists()) {
    	  FileUtil.logger.info("creating "+dotHome+" directory");
    	  dotHome.mkdir();
      }
      return dotHome;
    }

    private static String sep(String a, String b) { return a + FILE_SEPARATOR + b; }

}
