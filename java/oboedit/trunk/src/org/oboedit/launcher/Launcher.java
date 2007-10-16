package org.oboedit.launcher;

import java.beans.*;
import java.io.*;
import java.util.*;

import org.oboedit.gui.Preferences;

public class Launcher {

	public static void main(String[] args) throws Exception {
		String memsetting = Preferences.readMemStringFromDisk();

		System.err.println("Starting OBO-Edit using " + memsetting
				+ " of memory");

		List argList = new Vector();
		argList.add("java");
		if (memsetting != null)
			argList.add("-mx" + memsetting);
		argList.add("-jar");
		argList.add(Preferences.getInstallationDirectory()
				+ "/runtime/oboedit.jar");
		for (int i = 0; i < args.length; i++)
			argList.add(args[i]);
		String[] newargs = new String[argList.size()];
		Iterator it = argList.iterator();
		for (int i = 0; it.hasNext(); i++)
			newargs[i] = (String) it.next();
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
		
//		int exitVal = proc.waitFor();
//		System.exit(exitVal);
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
					System.out.println(line);
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}
		}
	}
}
