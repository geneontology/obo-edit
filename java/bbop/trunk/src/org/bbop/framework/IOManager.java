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

	protected class BlockingRunnable<INPUT_TYPE, OUTPUT_TYPE> implements
			Runnable {
		protected boolean failed = false;
		protected IOOperation<INPUT_TYPE, OUTPUT_TYPE> op;
		protected INPUT_TYPE input;
		protected JDialog dialog;

		public BlockingRunnable(IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
				INPUT_TYPE input, JDialog dialog) {
			this.op = op;
			this.input = input;
			this.dialog = dialog;
		}

		public void run() {
			failed = false;
			for (IOListener listener : new LinkedList<IOListener>(listeners)) {
				if (!listener.willExecuteOperation(new IOEvent<INPUT_TYPE>(
						this, op, input))) {
					failed = true;
				}
			}
			dialog.setVisible(false);
			dialog.dispose();
		}

		public boolean isFailed() {
			return failed;
		}

	}

	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			final IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			final INPUT_TYPE input, boolean fireEvents)
			throws DataAdapterException {
		if (fireEvents) {
			JDialog d = new JDialog((Frame) null, true);
			d.getContentPane().add(new JLabel("OBO-Edit: Working..."));
			d.setLocation(GUIManager.getManager().getFrame().getLocation());
			d.toBack();
			BlockingRunnable<INPUT_TYPE, OUTPUT_TYPE> runnable = new BlockingRunnable<INPUT_TYPE, OUTPUT_TYPE>(
					op, input, d);
			Thread thread = new Thread(runnable);
			thread.start();
			d.pack();
			d.toBack();
			d.setVisible(true);
			d.toBack();
			if (runnable.isFailed())
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
