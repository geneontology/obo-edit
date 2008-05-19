package org.oboedit.launcher;

import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.IOUtil;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryList;

import java.io.*;

import org.apache.log4j.*;

public class OBODiff {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBODiff.class);

	public static void main(String[] args) throws IOException,
			DataAdapterException {
		Vector filelist = new Vector();
		OBOAdapter historyAdapter = null;
		String outPath = null;
		File outFile = null;

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-t")) {
				i++;
				if (i >= args.length) {
					printUsage();
					System.exit(1);
				} else if (args[i].equals("text")) {
					// it's the default, so we'll handle it later
				} else if (args[i].equals("xml")) {
					historyAdapter = new XMLHistoryAdapter();
				}
			} else if (args[i].equals("-o")) {
				i++;
				if (i >= args.length) {
					printUsage();
					System.exit(1);
				}
				outPath = args[i];
			} else
				filelist.add(args[i]);
		}
		if (filelist.size() != 2)
			printUsage();
		else {
			OBOSession a = getHistory((String) filelist.get(0));
			OBOSession b = getHistory((String) filelist.get(1));
			HistoryList changes = HistoryGenerator.getHistory(a, b, null);
			if (historyAdapter == null)
				historyAdapter = new DefaultHistoryDumper();
			boolean printResults = false;
			if (outPath == null) {
				outFile = File.createTempFile("history", ".dump");
				outPath = outFile.getAbsolutePath();
				outFile.deleteOnExit();
				printResults = true;
			}
			FileAdapterConfiguration adapterConfig = new FileAdapterConfiguration();
			adapterConfig.setWritePath(outPath);
			historyAdapter.doOperation(OBOAdapter.WRITE_HISTORY,
					adapterConfig, changes);
			if (printResults) {
				FileInputStream fis = new FileInputStream(outFile);
				IOUtil.dumpAndClose(fis, System.out);
				outFile.delete();
			}
		}
	}

	public static OBOSession getHistory(String path)
			throws DataAdapterException {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(path);
		Object out = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		return (OBOSession) out;
	}

	public static void printUsage() {
		logger.info("Usage: obodiff file1 file2");
	}
}
