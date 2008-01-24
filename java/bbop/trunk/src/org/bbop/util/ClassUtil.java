package org.bbop.util;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ClassUtil {

	/**
	 * Finds resources that match an ANT-style path. The string ** is replaced
	 * by any number of nested directories, and the * string is replaced by one
	 * level of directory wildcards. A list of suffixes can be provided as well.
	 * If at least one suffix matches a given resource URL, the resource is
	 * returned.
	 * 
	 * @param path
	 * @param suffix
	 * @return
	 */
	public static List<URL> getResources(String path, String... suffix) {
		return getResources(ClassUtil.class.getClassLoader(), path, suffix);
	}

	/**
	 * Finds resources that match an ANT-style path. The string ** is replaced
	 * by any number of nested directories, and the * string is replaced by one
	 * level of directory wildcards. A list of suffixes can be provided as well.
	 * If at least one suffix matches a given resource URL, the resource is
	 * returned.
	 * 
	 * @param classLoader
	 *            the classLoader to check
	 * @param path
	 * @param suffix
	 * @return
	 */
	public static List<URL> getResources(ClassLoader classLoader, String path,
			String... suffix) {
		List<URL> resources;
		try {
			resources = getResources(classLoader);
		} catch (MalformedURLException e) {
			resources = new ArrayList<URL>();
		} catch (IOException e) {
			resources = new ArrayList<URL>();
		}
		Iterator<URL> it = resources.iterator();
		String regexp = path.replaceAll("\\*\\*", ".`").replaceAll("\\*",
				"[^/]`").replace('`', '*');
		Pattern p = Pattern.compile(regexp);
		while (it.hasNext()) {
			URL url = it.next();
			String upath = getFilePath(url);
			File file = new File(upath);
			Matcher m = p.matcher(file.getPath());
			if (!m.matches()) {
				it.remove();
				continue;
			}
			boolean matches = suffix.length == 0;
			for (String s : suffix) {
				if (file.getName().endsWith(s)) {
					matches = true;
					break;
				}
			}
			if (!matches)
				it.remove();
//			else
//				System.err.println("wow!");
		}
		return resources;
	}

	public static String getFilePath(URL url) {
		if (url.getProtocol().equals("jar")) {
			String urlStr = url.toString();
			int excIndex = urlStr.indexOf('!');
			if (excIndex >= 0)
				return urlStr.substring(excIndex + 1, urlStr.length());
		} else if (url.getProtocol().equals("file"))
			return url.getPath();
		return null;
	}

	public static List<URL> getResources(VectorFilter<URL> filter) {
		List<URL> resources = getResources();
		Iterator<URL> it = resources.iterator();
		while (it.hasNext()) {
			URL url = it.next();
			if (!filter.satisfies(url))
				it.remove();
		}
		return resources;
	}

	/**
	 * Returns all the resources in the current classloader
	 * 
	 * @return
	 */
	public static List<URL> getResources() {
		try {
			return getResources(ClassUtil.class.getClassLoader());
		} catch (MalformedURLException e) {
			return Collections.emptyList();
		} catch (IOException e) {
			return Collections.emptyList();
		}
	}

	public static List<URL> getResources(ClassLoader cl) throws IOException,
			MalformedURLException {
		List<URL> resources = new ArrayList<URL>();
		while (cl != null) {
			if (cl instanceof URLClassLoader) {
				URLClassLoader ucl = (URLClassLoader) cl;
				URL[] urls = ucl.getURLs();
				for (int i = 0; i < urls.length; i++) {
					URL url = urls[i];
					if (url.getFile().endsWith(".jar")) {
						listJarResources(new URL("jar:" + url.toExternalForm()
								+ "!/"), resources);
					} else if (url.getProtocol().equals("file")) {
						File file = new File(url.getFile());
						if (file.isDirectory()) {
							listDirResources(file, resources);
						}
					}
				}
			}
			cl = cl.getParent();
		}
		return resources;
	}

	protected static void listDirResources(File dir, List<URL> resources)
			throws MalformedURLException {
		File[] files = dir.listFiles();
		for (int i = 0; i < files.length; i++) {
			File file = files[i];
			resources.add(file.toURL());
			if (file.isDirectory()) {
				listDirResources(file, resources);
			}
		}
	}

	protected static void listJarResources(URL jarUrl, List<URL> resources)
			throws IOException, MalformedURLException {
		JarURLConnection jarConnection = (JarURLConnection) jarUrl
				.openConnection();

		for (Enumeration<JarEntry> entries = jarConnection.getJarFile()
				.entries(); entries.hasMoreElements();) {
			JarEntry entry = (JarEntry) entries.nextElement();
			resources.add(new URL(jarUrl, entry.getName()));
		}
	}

	protected static String readAFewChars(URL url) throws IOException {
		StringBuffer buf = new StringBuffer(10);
		Reader reader = new InputStreamReader(url.openStream());
		for (int i = 0; i < 10; i++) {
			int c = reader.read();
			if (c == -1) {
				break;
			}
			buf.append((char) c);
		}
		reader.close();
		return buf.toString();
	}
}
