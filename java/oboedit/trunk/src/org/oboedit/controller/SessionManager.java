package org.oboedit.controller;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bbop.swing.BackgroundUtil;
import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.EventUtil;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.OperationModel;
import org.obo.history.OperationWarning;
import org.obo.query.QueryEngine;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.ReasonerListener;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasoner;
import org.obo.reasoner.impl.OnTheFlyReasonerFactory;
import org.obo.reasoner.impl.ReasonerOperationModel;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.TermUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.Selection.PathCalcMode;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.gui.event.OntologyReloadListener;
import org.oboedit.gui.event.ReasonerStatusEvent;
import org.oboedit.gui.event.ReasonerStatusListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

public class SessionManager {
	protected static SessionManager manager;

	protected OBOSession session;

	protected Collection<HistoryListener> historyListeners = new LinkedList<HistoryListener>();

	protected Collection<ReasonerStatusListener> reasonerStatusListeners = new LinkedList<ReasonerStatusListener>();

	protected Collection<RootChangeListener> rootChangeListeners = new LinkedList<RootChangeListener>();

	protected Collection<OntologyReloadListener> ontologyReloadListeners = new LinkedList<OntologyReloadListener>();

	protected List<HistoryItem> redoHistoryItems = new LinkedList<HistoryItem>();

	protected Collection<ReasonerListener> reasonerListeners = new LinkedList<ReasonerListener>();

	protected ReasonerFactory reasonerFactory = new ForwardChainingReasonerFactory();

	protected ReasonedLinkDatabase reasoner;

	protected OperationModel reasonerOpModel;

	protected QueryEngine queryEngine;

	protected boolean unflushedChanges = false;

	protected boolean recacheInBackground = true;

	public SessionManager() {
		setSession(new OBOSessionImpl());
		setUseReasoner(Preferences.getPreferences().getUseReasoner());
	}

	public void addOntologyReloadListener(OntologyReloadListener listener) {
		ontologyReloadListeners.add(listener);
	}

	public void removeOntologyReloadListener(OntologyReloadListener listener) {
		ontologyReloadListeners.remove(listener);
	}

	public void reload() {
		for (OntologyReloadListener listener : ontologyReloadListeners) {
			listener.reload();
		}
	}

	public void addReasonerListener(ReasonerListener listener, boolean AWTThread) {
		if (AWTThread)
			listener = (ReasonerListener) EventUtil
					.getThreadSafeListener(listener);
		reasonerListeners.add(listener);
	}

	public QueryEngine getQueryEngine() {
		return queryEngine;
	}

	public void removeReasonerListener(ReasonerListener listener) {
		reasonerListeners.remove(listener);
	}

	public void markChangesFlushed() {
		this.unflushedChanges = false;
	}

	public boolean needsSave() {
		return unflushedChanges;
	}

	public static SessionManager getManager() {
		if (manager == null)
			manager = new SessionManager();
		return manager;
	}

	public void addReasonerStatusListener(ReasonerStatusListener listener) {
		reasonerStatusListeners.add(listener);
	}

	public void removeReasonerStatusListener(ReasonerStatusListener listener) {
		reasonerStatusListeners.remove(listener);
	}

	public void addHistoryListener(HistoryListener listener) {
		historyListeners.add(listener);
	}

	public void removeHistoryListener(HistoryListener listener) {
		historyListeners.remove(listener);
	}

	public OBOSession getSession() {
		return session;
	}

	/**
	 * Leaves the current session in place, but sends a change root event to all
	 * the registered listeners. This is useful if a massive change has occurred
	 * to the session that has not been achieved via the history session (such
	 * as an irreversible import).
	 */
	public void reloadSession() {
		queryEngine = new QueryEngine(session);
		redoHistoryItems.clear();
		if (getUseReasoner()) {
			initializeReasonerDatabase();
		}
		fireChangeRoot(new RootChangeEvent(this, session));
	}

	public void setSession(OBOSession session) {
		this.session = session;
		reloadSession();
	}

	public boolean getUseReasoner() {
		return Preferences.getPreferences().getUseReasoner();
	}

	public void setUseReasoner(final boolean useReasoner) {
		if (getUseReasoner() != useReasoner) {
			Preferences.getPreferences().setUseReasoner(useReasoner);
			fireReasonerStatusChange(new ReasonerStatusEvent(this, useReasoner));
			if (useReasoner)
				initializeReasonerDatabase();
			else
				clearReasonerDatabase();
		}
	}

	public void addRootChangeListener(RootChangeListener listener) {
		rootChangeListeners.add(listener);
	}

	public void removeRootChangeListener(RootChangeListener listener) {
		rootChangeListeners.remove(listener);
	}

	protected void fireChangeRoot(RootChangeEvent event) {
		for (RootChangeListener listener : rootChangeListeners) {
			listener.changeRoot(event);
		}
	}

	protected void fireHistoryApplied(HistoryAppliedEvent event) {
		for (HistoryListener listener : historyListeners) {
			long time = System.currentTimeMillis();
			listener.applied(event);
			time = System.currentTimeMillis() - time;
			System.err.println("fired history applied on " + listener + " in "
					+ time);
		}
	}

	protected void fireHistoryReversed(HistoryAppliedEvent event) {
		for (HistoryListener listener : historyListeners) {
			listener.reversed(event);
		}
	}

	protected void doApply(HistoryItem item) {
		doApply(item, true);
	}

	protected void doApply(final HistoryItem item, final boolean doSelect) {
		session.getCurrentHistory().addItem(item);
		OperationWarning warning = getOperationModel().apply(item);
		if (warning != null) {
			Object[] params = { item, warning };
			Logger.getLogger("org.oboedit.datamodel.history").log(
					Level.WARNING,
					"Warning message while trying to apply history item",
					params);
		}

		// if (getUseReasoner())
		// reasonerOpModel.apply(item);
		long time = System.currentTimeMillis();
		fireHistoryApplied(new HistoryAppliedEvent(this, item));
		System.err.println("fired history applied in "
				+ (System.currentTimeMillis() - time));
		if (GUIUtil.getPostSelection(item) != null && doSelect) {
			Selection selection = SelectionManager.resolveSelectionDanglers(
					session, GUIUtil.getPostSelection(item));
			if (SelectionManager.getManager().doPreSelectValidation(this))
				SelectionManager.setGlobalSelection(selection);
		}
	}

	public boolean canRedo() {
		return redoHistoryItems.size() > 0;
	}

	public boolean canUndo() {
		return session.getCurrentHistory().size() > 0;
	}

	public void apply(HistoryItem item) {
		apply(item, true);
	}

	public void apply(HistoryItem item, boolean doSelect) {
		doApply(item, doSelect);
		redoHistoryItems.clear();
		unflushedChanges = true;
	}

	protected void clearReasonerDatabase() {
		if (reasoner != null) {
			for (ReasonerListener listener : reasonerListeners) {
				reasoner.removeReasonerListener(listener);
			}
		}
		reasoner = null;
		reasonerOpModel = null;
	}

	public void setRecacheInBackground(boolean recacheInBackground) {
		this.recacheInBackground = recacheInBackground;
	}

	public boolean getRecacheInBackground() {
		return recacheInBackground;
	}

	protected ReasonerListener masterReasonerListener = new ReasonerListener() {

		public void reasoningFinished() {

			for (ReasonerListener listener : reasonerListeners) {
				listener.reasoningFinished();
			}
		}

		public void reasoningStarted() {

			for (ReasonerListener listener : reasonerListeners) {
				listener.reasoningStarted();
			}
		}
	};

	protected void initializeReasonerDatabase() {
		if (reasoner != null) {
			reasoner.removeReasonerListener(masterReasonerListener);
			session.getOperationModel().removeLockstepModel(reasonerOpModel);
		}
		reasoner = createReasoner();
		reasoner.setLinkDatabase(session.getLinkDatabase());
		reasonerOpModel = new ReasonerOperationModel(reasoner);
		reasonerOpModel.setSession(session);
		session.getOperationModel().addLockstepModel(reasonerOpModel);
		reasoner.addReasonerListener(masterReasonerListener);

		/*
		 * if (recacheInBackground) { SwingWorker<Void, Void> worker = new
		 * SwingWorker<Void, Void>() {
		 * 
		 * @Override protected Void doInBackground() throws Exception {
		 * reasoner.recache(); return null; } }; worker.execute(); } else
		 * reasoner.recache();
		 */
		AbstractTaskDelegate<Void> reasoningTask = new AbstractTaskDelegate<Void>() {
			@Override
			public void execute() {
				System.err.println("reasoning started...");
				long time = System.currentTimeMillis();
				reasoner.recache();
				System.err.println("reasoning finished in "
						+ (System.currentTimeMillis() - time));
			}

			@Override
			public Number getProgressValue() {
				return reasoner.getProgressValue();
			}

			@Override
			public String getProgressString() {
				return reasoner.getProgressString();
			}

			@Override
			public void cancel() {
				reasoner.cancel();
				super.cancel();
				setUseReasoner(false);
			}
		};
		if (getRecacheInBackground())
			BackgroundUtil.scheduleTask(reasoningTask);
		else
			try {
				reasoningTask.execute();
			} catch (Exception ex) {
				Logger.getLogger("").log(Level.SEVERE,
						"Failed to create reasoner", ex);
			}
	}

	public ReasonedLinkDatabase createReasoner() {
		return reasonerFactory.createReasoner();
	}

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	/**
	 * If the reasoner is enabled, return the results of {@link #getReasoner()},
	 * otherwise return getSession().getLinkDatabase().
	 * 
	 * @return
	 */
	public LinkDatabase getCurrentFullLinkDatabase() {
		if (getUseReasoner()) {
			return getReasoner();
		} else
			return session.getLinkDatabase();

	}

	public LinkDatabase getCurrentLinkDatabase() {
		if (getUseReasoner())
			return new TrimmedLinkDatabase(reasoner);
		else
			return session.getLinkDatabase();
	}

	public HistoryItem getRedoItem() {
		return (HistoryItem) redoHistoryItems.get(redoHistoryItems.size() - 1);
	}

	public HistoryItem getUndoItem() {
		HistoryList historyList = session.getCurrentHistory();
		return historyList.getItemAt(historyList.size() - 1);
	}

	public void applyList(HistoryList list) {
		Iterator it = list.getHistoryItems();
		HistoryItem item = null;
		while (it.hasNext()) {
			item = (HistoryItem) it.next();
			session.getCurrentHistory().addItem(item);
			OperationWarning warning = getOperationModel().apply(item);
			if (getUseReasoner()) {
				OperationWarning reasonerWarning = reasonerOpModel.apply(item);
				Object[] params = { item, reasonerWarning };
				Logger.getLogger("org.oboedit.datamodel.history").log(
						Level.WARNING,
						"Warning message while trying to apply history item",
						params);
			}
			if (warning != null) {
				Object[] params = { item, warning };
				Logger.getLogger("org.oboedit.datamodel.history").log(
						Level.WARNING,
						"Warning message while trying to apply history item",
						params);

				System.err.println("*** GOT WARNING = " + warning);
			}
			fireHistoryApplied(new HistoryAppliedEvent(this, item));
		}

		SelectionManager.setGlobalSelection(GUIUtil.getPostSelection(item));
	}

	public OperationModel getOperationModel() {
		return session.getOperationModel();
	}

	protected void reverse(HistoryItem item) {
		OperationWarning warning = getOperationModel().reverse(item);
		if (warning != null)
			System.err.println("warning = " + warning);
		if (getUseReasoner()) {
			reasonerOpModel.reverse(item);
		}
		fireHistoryReversed(new HistoryAppliedEvent(this, item));

		SelectionManager.setGlobalSelection(GUIUtil.getPreSelection(item));
	}

	public void redo() {
		HistoryItem item = (HistoryItem) redoHistoryItems
				.remove(redoHistoryItems.size() - 1);
		doApply(item);
	}

	public void undo() {
		HistoryList historyList = session.getCurrentHistory();
		HistoryItem item = historyList.getItemAt(historyList.size() - 1);
		historyList.removeItem(item);
		reverse(item);
		redoHistoryItems.add(item);
	}

	protected void fireReasonerStatusChange(ReasonerStatusEvent e) {
		for (ReasonerStatusListener listener : reasonerStatusListeners) {
			listener.statusChanged(e);
		}
	}

	public ReasonerFactory getReasonerFactory() {
		return reasonerFactory;
	}

	public void setReasonerFactory(ReasonerFactory reasonerFactory) {
		this.reasonerFactory = reasonerFactory;
	}
}
