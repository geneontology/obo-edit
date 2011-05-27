package org.oboedit.launcher;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;
import org.bbop.io.ExtensionFilenameFilter;
import org.oboedit.gui.Preferences;

public class Launcher {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Launcher.class);

	public static void main(String[] args) throws Exception {
		Preferences.setBatchMode(false);
		String memsetting = Preferences.readMemStringFromDisk();

		logger.info("Starting OBO-Edit using " + memsetting + " of memory specified in the OBO-Edit.vmoptions file");

		List<String> argList = new LinkedList<String>();
		argList.add("java");
		if (memsetting != null)
			argList.add("-Xmx" + memsetting);
		StringBuffer classpath = new StringBuffer();
		File runtimeDir = new File(Preferences.getInstallationDirectory(),
				"runtime");
		
		for (File jarFile : runtimeDir.listFiles(new ExtensionFilenameFilter(
				"jar"))) {
			if (classpath.length() > 0)
				classpath.append(System.getProperty("path.separator"));
			String s = jarFile.toString().replace(
					System.getProperty("path.separator"),
					"\\" + System.getProperty("path.separator"));
			classpath.append(s);
		}
		argList.add("-classpath");
		argList.add(classpath.toString());
		argList.add("org.oboedit.launcher.OBOEdit");
		logger.info(argList);
		for (int i = 0; i < args.length; i++)
			argList.add(args[i]);
		String[] newargs = new String[argList.size()];
		Iterator<String> it = argList.iterator();
		for (int i = 0; it.hasNext(); i++)
			newargs[i] = it.next();
		Runtime rt = Runtime.getRuntime();
		Process proc = rt.exec(newargs);

		StreamGobbler errorGobbler = new StreamGobbler(proc.getErrorStream(),
				"ERROR");

		StreamGobbler outputGobbler = new StreamGobbler(proc.getInputStream(),
				"OUTPUT");

		errorGobbler.start();
		outputGobbler.start();

		// The code below was removed to try to hide the 2 launcher
		// icons on MacOS

		// int exitVal = proc.waitFor();
		// System.exit(exitVal);
	}

	protected static class StreamGobbler extends Thread {
		InputStream is;
		String type;

		StreamGobbler(InputStream is, String type) {
			this.is = is;
			this.type = type;
		}

		@Override
		public void run() {
			try {
				InputStreamReader isr = new InputStreamReader(is);
				BufferedReader br = new BufferedReader(isr);
				String line = null;
				while ((line = br.readLine()) != null)
					logger.info(line);
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}
		}
	}
}
