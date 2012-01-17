package org.bbop.framework;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;


import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.DefaultAdapterRegistry;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.dataadapter.IOOperation;
import org.bbop.io.FileUtil;
import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.BackgroundUtil;
import org.bbop.util.AbstractTaskDelegate;

import org.apache.log4j.*;

public class IOManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IOManager.class);
	

	protected static IOManager manager;

	protected Collection<IOListener> listeners = new LinkedList<IOListener>();

	protected DataAdapterRegistry adapterRegistry;

        protected DataAdapter currentAdapter = null;

	protected String historyPath;

	public void setHistoryFilePath(String historyPath) {
		this.historyPath = historyPath;
	}

	public String getHistoryFilePath() {
	    if (historyPath == null) {
		File historyFile = new File(GUIManager.getPrefsDir(), "history.xml");
//		logger.info("getHistoryFilePath: no existing history in " + historyFile.getAbsolutePath() + "--creating from resource org/oboedit/resources/history.xml."); // DEL
		// If there's no history yet, use the default one stored as a resource, and copy to .oboedit directory
		try {
		    FileUtil.ensureExists(historyFile,
					  "org/oboedit/resources/history.xml");
		    historyPath = historyFile.getAbsolutePath();
		} catch (IOException e) {
		    logger.error("ensureExists failed: " + e);
		}
	    }
	    return historyPath;
	}

	public DataAdapterRegistry getAdapterRegistry() {
		return adapterRegistry;
	}

	public void setAdapterRegistry(DataAdapterRegistry adapterRegistry) {
		this.adapterRegistry = adapterRegistry;
	}

	public static IOManager getManager() {
		if (manager == null)
			manager = new IOManager();
		return manager;
	}

	protected void fireOperationExecuted(IOEvent<?> e) {
		for (IOListener listener : new LinkedList<IOListener>(listeners)) {
			listener.operationExecuted(e);
		}
	}

	public void addIOListener(IOListener listener) {
		listeners.add(listener);
	}

	public void removeIOListener(IOListener listener) {
		listeners.remove(listener);
	}

	protected IOManager() {
		adapterRegistry = new DefaultAdapterRegistry();
	}

	public void installDataAdapter(DataAdapter adapter) {
		adapterRegistry.addAdapter(adapter);
	}

	public void removeDataAdapter(DataAdapter adapter) {
		adapterRegistry.removeAdapter(adapter);
	}

	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			final IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			final INPUT_TYPE input, boolean fireEvents)
			throws DataAdapterException {
		if (fireEvents) {
			AbstractTaskDelegate<Boolean> eventTask = new AbstractTaskDelegate<Boolean>() {

				@Override
				public void execute() throws Exception {
					for (IOListener listener : new LinkedList<IOListener>(
							listeners)) {
						if (!listener
								.willExecuteOperation(new IOEvent<INPUT_TYPE>(
										this, op, input))) {
							setResults(true);
							return;
						}
					}
					setResults(false);
				}

			};
                        // Why is this needed, anyway? It sometimes gets in the way.
			BackgroundEventQueue queue = new BackgroundEventQueue();
			BackgroundUtil.scheduleTask(queue, eventTask,
						    true, "OBO-Edit: IOManager working");
			queue.die();
			if (eventTask.getResults())
				return null;
		}
		DataAdapterRegistry registry = getAdapterRegistry();
		final GraphicalAdapterChooser<INPUT_TYPE, OUTPUT_TYPE> gac = new GraphicalAdapterChooser<INPUT_TYPE, OUTPUT_TYPE>(
				registry, op, GUIManager.getManager().getScreenLockQueue(),
				GUIManager.getManager().getFrame(), true, input);
		gac.setHistoryPath(getHistoryFilePath());
		boolean worked = gac.showDialog(op.getName(), GUIManager.getManager()
				.getFrame());
		if (worked) {
                    setCurrentAdapter(gac.getCurrentAdapter());
                    return gac.getResult();
                }
		else
			return null;
	}

    public DataAdapter getCurrentAdapter() {
        return currentAdapter;
    }
    private void setCurrentAdapter(DataAdapter ca) {
        this.currentAdapter = ca;
    }
}
