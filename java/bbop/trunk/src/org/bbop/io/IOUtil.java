package org.bbop.io;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.zip.*;
import org.apache.log4j.*;

public final class IOUtil {
	public static void copyFile(File in, File out) throws IOException {
	
//		initialize logger
		final Logger logger = Logger.getLogger(IOUtil.class);
		FileInputStream fis = new FileInputStream(in);
		FileOutputStream fos = new FileOutputStream(out);
		byte[] buf = new byte[1024];
		int i = 0;
		while ((i = fis.read(buf)) != -1) {
			fos.write(buf, 0, i);
		}
		fis.close();
		fos.close();
	}
	
	public static String readFileToPosition(String uri, int pos) {
		try {
			InputStream s = new BufferedInputStream(getStream(uri));
			StringBuffer out = new StringBuffer();
			int c;
			int byteCount = 0;
			while((c = s.read()) != -1) {
				out.append((char) c);
				byteCount++;
				if (pos >= 0 && byteCount >= pos) {
					break;
				}
			}
			s.close();
			return out.toString();
		} catch (IOException ex) {
			return null;
		}
	}
	
	public static String readFile(String uri) {
		return readFileToPosition(uri, -1);
	}

	public static long calculateChecksum(InputStream stream) throws IOException {
		CRC32 crc32 = new CRC32();
		int read;
		while ((read = stream.read()) != -1) {
			crc32.update(read);
		}
		return crc32.getValue();
	}

	public static File createTempDir(String name, String ext) throws Exception {
		File f = File.createTempFile(name, ext);
		f.delete();
		f.mkdirs();
		return f;
	}

	public static void deltreeOnExit(final File file) throws IOException {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {
				try {
					deltree(file);
				} catch (Exception ex) {
				}
			}
		});
	}

	public static void deltree(File file) throws IOException {
		//logger.info("file = " + file);
		if (!file.exists())
			return;
		if (file.isDirectory()) {
			String[] children = file.list();
			for (int i = 0; i < children.length; i++)
				deltree(new File(file, children[i]));
			file.delete();
		} else if (file.isFile())
			file.delete();
	}

	public static void main(String[] args) throws Exception {

		ZipFile f = new ZipFile(new File(args[0]));
		File tempDir = createTempDir("archive", ".dir");
		//logger.info("created " + tempDir);
		File outFile = new File("/home/jrichter/zip");
		unzip(outFile, f);
		deltree(outFile);
	}

	public static void unzip(File targetDir, File file) throws IOException {
		unzip(targetDir, new ZipFile(file));
	}

	public static void unzip(File targetDir, ZipFile file) throws IOException {
		Enumeration e = file.entries();
		while (e.hasMoreElements()) {
			ZipEntry entry = (ZipEntry) e.nextElement();
			if (entry.isDirectory()) {
				File dir = new File(entry.getName());
				dir.mkdirs();
			} else {
				File outFile = new File(targetDir, entry.getName());
				if (outFile.getParentFile() != null)
					outFile.getParentFile().mkdirs();
				InputStream istream = new BufferedInputStream(file
						.getInputStream(entry));
				OutputStream ostream = new BufferedOutputStream(
						new FileOutputStream(outFile));
				IOUtil.dumpAndClose(istream, ostream);
			}
		}
	}

	public static void dumpAndClose(InputStream istream, OutputStream ostream)
			throws IOException {
		int read;
		while ((read = istream.read()) != -1) {
			ostream.write((byte) read);
		}
		istream.close();
		ostream.close();
	}

	public static ProgressableInputStream getProgressableStream(String uri)
			throws IOException {
		File file = new File(uri);
		String pathStr = null;
		InputStream stream;
		if (file.exists()) {
			return new ProgressableFileInputStream(file);
		} else {
			URL url = null;
			try {
				url = new URL(uri);
				return new ProgressableURLInputStream(url);
			} catch (MalformedURLException ex) {
				throw new IOException("Invalid path " + uri);
			}
		}
	}

	public static InputStream getStream(String uri) throws IOException {
		File file = new File(uri);
		String pathStr = null;
		InputStream stream;
		if (file.exists()) {
			stream = new FileInputStream(file);
			pathStr = file.toString();
		} else {
			URL url = null;
			try {
				url = new URL(uri);
			} catch (MalformedURLException ex) {
				throw new IOException("Invalid path " + uri);
			}
			pathStr = url.toString();
			stream = url.openStream();
		}
		return new BufferedInputStream(stream);
	}

	public static String getShortName(String path) {
		URL url = getURL(path);
		if (url == null)
			return null;
		File file = new File(url.getPath());
		return file.getName();
	}

	public static URL getURL(String path) {
		try {
			URL url = new URL(path);
			File file = new File(path);
			if (file.exists())
				return new URL("file:" + file.getCanonicalPath());
			return url;
		} catch (MalformedURLException e) {
			try {
				File file = new File(path);
				return new URL("file:" + file.getCanonicalPath());
			} catch (MalformedURLException ex) {
				return null;
			} catch (IOException ex) {
				return null;
			}
		} catch (IOException e) {
			return null;
		}
	}

	public static boolean isURL(String path) {
		try {
			URL url = new URL(path);
			return true;
		} catch (MalformedURLException ex) {
			return false;
		}
	}

/**
 * This function will copy files or directories from one location to another.
 * note that the source and the destination must be mutually exclusive. This 
 * function can not be used to copy a directory to a sub directory of itself.
 * The function will also have problems if the destination files already exist.
 * From http://www.dreamincode.net/code/snippet1443.htm
 * @param src -- A File object that represents the source for the copy
 * @param dest -- A File object that represnts the destination for the copy.
 * @throws IOException if unable to copy.
 */
	public static void copyFiles(File src, File dest) throws IOException {
		//Check to ensure that the source is valid...
		if (!src.exists()) {
			throw new IOException("copyFiles: Can not find source: " + src.getAbsolutePath()+".");
		} else if (!src.canRead()) { //check to ensure we have rights to the source...
			throw new IOException("copyFiles: No right to source: " + src.getAbsolutePath()+".");
		}
		//is this a directory copy?
		if (src.isDirectory()) {
			if (!dest.exists()) { //does the destination already exist?
				//if not we need to make it exist if possible (note this is mkdirs not mkdir)
				if (!dest.mkdirs()) {
					throw new IOException("copyFiles: Could not create direcotry: " + dest.getAbsolutePath() + ".");
				}
			}
			//get a listing of files...
			String list[] = src.list();
			//copy all the files in the list.
			for (int i = 0; i < list.length; i++)
			{
				File dest1 = new File(dest, list[i]);
				File src1 = new File(src, list[i]);
				copyFiles(src1 , dest1);
			}
		} else { 
			//This was not a directory, so lets just copy the file
			try {
				System.out.println("copyFile " + src + " -> " + dest);  // DEL
				copyFile(src, dest);
			} catch (IOException e) { //Error copying file... 
				IOException wrapper = new IOException("copyFiles: Unable to copy file: " + 
								      src.getAbsolutePath() + "to" + dest.getAbsolutePath()+".");
				wrapper.initCause(e);
				wrapper.setStackTrace(e.getStackTrace());
				throw wrapper;
			}
		}
	}
}
