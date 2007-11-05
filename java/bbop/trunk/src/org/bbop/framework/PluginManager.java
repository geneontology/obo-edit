package org.bbop.framework;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bbop.io.IOUtil;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

public class PluginManager {

	protected static PluginManager manager;

	protected File[] pluginDirs;

	protected boolean initialized = false;

	protected Map<URL, ClassLoader> classLoaderMap = new HashMap<URL, ClassLoader>();

	protected Collection<URL> pluginJars;

	protected MultiMap<URL, Class<?>> classMap = new MultiHashMap<URL, Class<?>>();

	protected static FilenameFilter jarFilter = new FilenameFilter() {

		public boolean accept(File dir, String name) {
			return name.endsWith(".jar");
		}
	};

	public static class InstallerConfig {

		protected Collection<String> libs = new LinkedList<String>();

		protected Collection<String> classes = new LinkedList<String>();

		public InstallerConfig(Collection<String> classes,
				Collection<String> libs) {
			setClasses(classes);
			setLibs(libs);
		}

		public InstallerConfig() {

		}

		public Collection<String> getLibs() {
			return libs;
		}

		public void setLibs(Collection<String> libs) {
			this.libs = libs;
		}

		public Collection<String> getClasses() {
			return classes;
		}

		public void setClasses(Collection<String> classes) {
			this.classes = classes;
		}
	}

	public static void createInstallerConfiguration(Collection<String> classes,
			Collection<String> libs, File dest) throws FileNotFoundException {
		InstallerConfig config = new InstallerConfig();
		config.setClasses(classes);
		config.setLibs(libs);
		XMLEncoder encoder = new XMLEncoder(new FileOutputStream(dest));
		encoder.writeObject(config);
		encoder.close();
	}

	public <T> Collection<T> instantiateAll(Class<T> makeMe) {
		LinkedList<T> out = new LinkedList<T>();
		for (Class<?> c : getClasses()) {
			if (makeMe.isAssignableFrom(c)) {
				Class<T> tc = (Class<T>) c;
				try {
					Constructor<T> con = tc.getConstructor();
					out.add(con.newInstance());
				} catch (IllegalArgumentException e) {
				} catch (InstantiationException e) {
				} catch (IllegalAccessException e) {
				} catch (InvocationTargetException e) {
				} catch (SecurityException e) {
				} catch (NoSuchMethodException e) {
				}

			}
		}
		return out;
	}

	public static PluginManager getManager() {
		if (manager == null) {
			manager = new PluginManager();
		}
		return manager;
	}

	public File[] getPluginDirs() {
		return pluginDirs;
	}

	public void setPluginDirs(File... pluginDirs) {
		this.pluginDirs = pluginDirs;
	}

	public Collection<Class<?>> getClasses() {
		if (!initialized)
			init();
		return classMap.singleValues();
	}

	public Collection<Class<?>> getClasses(URL url) {
		if (!initialized)
			init();
		return classMap.get(url);
	}

	public Collection<URL> getPluginJars() {
		if (!initialized)
			init();
		return pluginJars;
	}

	public ClassLoader getClassLoader(URL url) {
		if (!initialized)
			init();
		return classLoaderMap.get(url);
	}

	public static void main(String[] args) throws Exception {

		LinkedList<String> list = new LinkedList<String>();
		list.add("edu.berkeley.oboplugins.MoreInfoPlugin");
		PluginManager.createInstallerConfiguration(list, null, new File(
				"/Users/jrichter/workspace/PluginProject/classes/install.xml"));

		PluginManager.getManager().setPluginDirs(
				new File("/Users/jrichter/testjars"));
		Collection<GUITask> tasks = PluginManager.getManager().instantiateAll(
				GUITask.class);
		System.err.println("tasks = " + tasks);
	}

	protected void init() {
		initialized = true;

		ClassLoader parentLoader = PluginManager.class.getClassLoader();
		LinkedList<URL> classPathJars = new LinkedList<URL>();
		MultiMap<URL, URL> embeddedJars = new MultiHashMap<URL, URL>();
		LinkedList<URL> pluginJars = new LinkedList<URL>();
		Map<URL, JarFile> classSearchJarFiles = new HashMap<URL, JarFile>();
		MultiMap<URL, String> classLoadJarFiles = new MultiHashMap<URL, String>();
		for (File f : getPluginDirs()) {
			if (!f.exists() || !f.isDirectory())
				continue;
			File sharedDir = new File(f, "shared");
			if (sharedDir.exists() && sharedDir.isDirectory()) {
				LinkedList<URL> sharedLibs = new LinkedList<URL>();
				for (File d : sharedDir.listFiles(jarFilter)) {
					try {
						new JarFile(d);
						sharedLibs.add(d.toURL());
					} catch (IOException e) {
						System.err.println("Could not read shared jar " + d);
					}
				}
				if (sharedLibs.size() > 0) {
					parentLoader = new URLClassLoader(sharedLibs
							.toArray(new URL[sharedLibs.size()]));
				}
			}
			File[] temp = f.listFiles(jarFilter);
			for (File jar : temp) {
				try {
					JarFile jf = new JarFile(jar);
					classPathJars.add(jar.toURL());
					pluginJars.add(jar.toURL());
					URL[] urls = { jar.toURL() };
					URLClassLoader pluginLoader = new URLClassLoader(urls);
					boolean doLibrarySearch = true;
					boolean doClassSearch = true;
					URL classIndexFile = pluginLoader
							.getResource("install.xml");
					if (classIndexFile != null) {
						XMLDecoder decoder = new XMLDecoder(classIndexFile
								.openStream());
						InstallerConfig config = (InstallerConfig) decoder
								.readObject();
						if (config != null) {
							String file = null;
							try {
								if (config.getLibs() != null) {
									for (String s : config.getLibs()) {
										file = s;
										URL embeddedURL = pluginLoader
												.getResource(s);
										if (embeddedURL != null)
											embeddedJars.add(jar.toURL(),
													embeddedURL);
									}
									doLibrarySearch = false;
								}
							} catch (Throwable t) {
								Logger logger = Logger.getLogger("plugins");
								logger.log(Level.WARNING,
										"Unable to load embedded library "
												+ file, t);
							}
							String className = null;
							if (config.getClasses() != null) {
								for (String s : config.getClasses()) {
									className = s;
									classLoadJarFiles.add(jar.toURL(), s);
								}
								doClassSearch = false;
							}
						}
					}
					if (doLibrarySearch) {
						JarEntry libEntry = jf.getJarEntry("lib/");
						if (libEntry != null) {
							if (libEntry.isDirectory()) {
								java.util.Enumeration<JarEntry> e = jf
										.entries();
								while (e.hasMoreElements()) {
									JarEntry je = e.nextElement();
									if (je.getName().startsWith("lib/")
											&& je.getName().endsWith(".jar")) {
										URL embeddedURL = pluginLoader
												.getResource(je.getName());
										embeddedJars.add(jar.toURL(),
												embeddedURL);
									}
								}
							}
						}
					}
					if (doClassSearch)
						classSearchJarFiles.put(jar.toURL(), jf);
				} catch (IOException e) {
					Logger logger = Logger.getLogger("plugins");
					logger.log(Level.WARNING, "Could not read plugin jar "
							+ jar, e);
				}
			}
		}
		this.pluginJars = pluginJars;
		for (URL url : classPathJars) {
			LinkedList<URL> urlList = new LinkedList<URL>();
			for (URL embedded : embeddedJars.get(url)) {
				try {
					File temp = File.createTempFile("templib", ".jar");
					temp.deleteOnExit();
					IOUtil.dumpAndClose(embedded.openStream(),
							new FileOutputStream(temp));
					urlList.add(temp.toURL());
				} catch (IOException e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				}
			}
			urlList.add(url);
			URLClassLoader loader = new URLClassLoader(urlList
					.toArray(new URL[urlList.size()]), parentLoader);
			classLoaderMap.put(url, loader);
		}
		URL[] urls = classPathJars.toArray(new URL[classPathJars.size()]);
		for (URL url : classSearchJarFiles.keySet()) {
			ClassLoader pluginLoader = getClassLoader(url);
			JarFile jf = classSearchJarFiles.get(url);
			java.util.Enumeration<JarEntry> e = jf.entries();
			while (e.hasMoreElements()) {
				JarEntry je = e.nextElement();
				if (!je.getName().startsWith("lib/")
						&& je.getName().endsWith(".class")
						&& je.getName().length() > 6) {
					String name = je.getName().replace('/', '.').substring(0,
							je.getName().length() - 6);
					try {
						Class<?> c = pluginLoader.loadClass(name);
						classMap.add(url, c);
					} catch (ClassNotFoundException e1) {
						Logger logger = Logger.getLogger("plugins");
						logger.log(Level.WARNING, "Could not read class file "
								+ name + " during automatic search of jar "
								+ url, e1);
					}
				}
			}
		}
		for (URL url : classLoadJarFiles.keySet()) {
			ClassLoader pluginLoader = getClassLoader(url);
			for (String s : classLoadJarFiles.get(url)) {
				try {
					Class<?> c = pluginLoader.loadClass(s);
					classMap.add(url, c);
				} catch (ClassNotFoundException e1) {
					Logger logger = Logger.getLogger("plugins");
					logger.log(Level.WARNING, "Could not read class file " + s
							+ " specified in installation preferences of jar "
							+ url, e1);
				}
			}
		}
	}

	private PluginManager() {

	}
}
