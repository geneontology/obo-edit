package org.bbop.framework;

import java.awt.Frame;
import java.io.File;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.DefaultAdapterRegistry;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.dataadapter.IOOperation;
import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.BackgroundUtil;
import org.bbop.util.AbstractTaskDelegate;

public class IOManager {

	protected static IOManager manager;

	protected Collection<IOListener> listeners = new LinkedList<IOListener>();

	protected DataAdapterRegistry adapterRegistry;

	protected String historyPath;

	public void setHistoryFilePath(String historyPath) {
		this.historyPath = historyPath;
	}

	public String getHistoryFilePath() {
		if (historyPath == null)
			historyPath = new File(GUIManager.getPrefsDir(), "history.xml")
					.getAbsolutePath();
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
			BackgroundEventQueue queue = new BackgroundEventQueue();
			BackgroundUtil.scheduleTask(queue, eventTask,
					true, "OBO-Edit: Working");
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
		if (worked)
			return gac.getResult();
		else
			return null;
	}
}
