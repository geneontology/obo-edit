package org.bbop.io;

import java.io.*;
import java.lang.reflect.*;

public class FileUtil {

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
    public static void ensureExists(File file, ClassLoader loader,
				    String resource)
	throws IOException {

	if(!file.exists()) {
	    if (loader == null) {
		throw new IOException("ClassLoader is null,"+
				      " can't load resource.");
	    }

	    if (resource == null) {
	    	throw new NullPointerException("Can't load from null resource");
	    }
	    InputStream is = loader.getResourceAsStream(resource);
	    if(is == null) {
		throw new FileNotFoundException("Couldn't find resource "+
						resource);
	    }
	    (new File(file.getParent())).mkdirs();
	    OutputStream os = new BufferedOutputStream(
						  new FileOutputStream(file));
	    
	    System.err.println("ensureExists: creating " + file.getAbsolutePath() + " from resource " + resource); // DEL
	    for(int next = is.read(); next != -1; next = is.read()) {
		os.write(next);
	    }
	    
	    os.flush();
	    os.close();
	}
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
}
