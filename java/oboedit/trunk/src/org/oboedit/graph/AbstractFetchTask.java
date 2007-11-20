package org.oboedit.graph;

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;

import javax.swing.SwingUtilities;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUITask;
import org.bbop.framework.event.GUIComponentEvent;
import org.bbop.framework.event.GUIComponentListener;
import org.bbop.util.ComparableComparator;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.filters.AbstractBooleanCriterion;
import org.obo.filters.AbstractCriterion;
import org.obo.filters.AbstractNumberCriterion;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.components.GraphEditor;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.event.ExpandCollapseListener;
import org.oboedit.gui.event.ExpansionEvent;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.RenderedFilter;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

/**
 * An abstract superclass for ViewBehaviors that need to perform some slow
 * calculation or data retrieval each time one or more new nodes are displayed
 * by a LinkDatabaseCanvas. When the calculation/data retrieval is finished for
 * every newly displayed node, the LinkDatabaseCanvas is re-laid-out and
 * redecorated. The renderers given by {@link #getPendingRenderer()}
 * {@link #getFetchedRenderer()} will be used to render the new nodes
 * 
 * @author jrichter
 * 
 * @param <T>
 *            The datatype of the value fetched by
 *            {@link #getValue(IdentifiedObject)}
 */
public abstract class AbstractFetchTask<T> implements GUITask {

	protected static int autogen_id = 0;

	protected class PerPanelTask {

		protected String id;
		protected FilteredRenderable canvas;
		protected RenderedFilter fetchedRenderFilter;
		protected RenderedFilter pendingRenderFilter;

		protected class GeneralFetchThread extends Thread {
			protected boolean cancelled = false;

			public void cancel() {
				cancelled = true;
			}

			public void run() {
				while (!cancelled) {
					// Collection<FetchThread> threads = new
					// ArrayList<FetchThread>();
					// while (pendingObjects.size() > 0) {
					// IdentifiedObject io = pendingObjects.removeFirst();
					// decoratedObjects.put(io, getValue(io));
					// System.err.println("decoratedObjects =
					// "+decoratedObjects);
					// canvas.relayout();
					// }
					while (pendingObjects.size() > 0) {
						Collection<FetchThread> threads = new ArrayList<FetchThread>();
						while (pendingObjects.size() > 0) {
							IdentifiedObject io = pendingObjects.removeFirst();
							FetchThread ft = new FetchThread(io);
							threads.add(ft);
							ft.start();
						}
						for (FetchThread thread : threads)
							try {
								thread.join();
							} catch (InterruptedException e1) {
							}
						for (FetchThread thread : threads) {
							decoratedObjects.put(thread.getIdentifiedObject(),
									thread.getResult());
						}
					}
					SwingUtilities.invokeLater(new Runnable() {
						public void run() {
							canvas.redraw();
						}
					});
					try {
						join();
					} catch (InterruptedException e) {
					}
				}
			}
		}

		protected LinkedList<IdentifiedObject> pendingObjects = new LinkedList<IdentifiedObject>();

		protected GeneralFetchThread thread;

		public PerPanelTask() {
			id = "" + autogen_id++;
		}

		protected ExpandCollapseListener listener = new ExpandCollapseListener() {

			public void expandStateChanged(ExpansionEvent e) {
				pendingObjects.addAll(e.getShown());
				decoratedObjects.keySet().removeAll(e.getHidden());
				pendingObjects.removeAll(e.getHidden());
				thread.interrupt();
			}

		};

		protected boolean isPending(IdentifiedObject io) {
			return pendingObjects.contains(io);
		}

		protected boolean isDecorated(IdentifiedObject io) {
			return decoratedObjects.containsKey(io);
		}

		public void install(FilteredRenderable canvas) {
			this.canvas = canvas;
			canvas.addExpansionListener(listener);
			thread = new GeneralFetchThread();
			thread.start();
			GeneralRendererSpec fetchedSpec = getFetchedRenderer(canvas,
					getValueVarName());
			if (fetchedSpec != null) {
				ObjectFilter filter = new ObjectFilterImpl();
				filter.setCriterion(isDecoratedCriterion);
				fetchedRenderFilter = new RenderedFilter(filter, fetchedSpec);
				canvas.addAutomaticObjectRenderer(fetchedRenderFilter);
			}
			GeneralRendererSpec pendingSpec = getPendingRenderer(canvas,
					getValueVarName());
			if (pendingSpec != null) {
				ObjectFilter filter = new ObjectFilterImpl();
				filter.setNegate(true);
				filter.setCriterion(isDecoratedCriterion);
				pendingRenderFilter = new RenderedFilter(filter, pendingSpec);
				canvas.addAutomaticObjectRenderer(pendingRenderFilter);
			}
		}

		public void shutdown(FilteredRenderable canvas) {
			thread.cancel();
			canvas.removeExpansionListener(listener);
			if (fetchedRenderFilter != null)
				canvas.removeAutomaticObjectRenderer(fetchedRenderFilter);
			if (pendingRenderFilter != null)
				canvas.removeAutomaticObjectRenderer(pendingRenderFilter);
		}

	}

	protected Comparator comparator = ComparableComparator.COMPARATOR;
	protected Map<IdentifiedObject, T> decoratedObjects = new HashMap<IdentifiedObject, T>();

	protected Class<T> valueClass;

	public AbstractFetchTask(Class<T> valueClass) {
		this.valueClass = valueClass;
	}

	/**
	 * Actually returns the value fetched by this view behavior. The graph will
	 * be redrawn when every pending fetch has completed.
	 * 
	 * @param io
	 * @return
	 */
	protected abstract T getValue(IdentifiedObject io);

	/**
	 * An identifier for this view behavior. This identifier will be used as a
	 * prefix for the identifiers of renderers and search criteria created by
	 * this behavior.
	 * 
	 * @return
	 */
	protected abstract String getBehaviorID();

	/**
	 * The renderer to use for nodes whose decoration data is still being
	 * fetched
	 * 
	 * @return
	 */
	protected abstract GeneralRendererSpec getPendingRenderer(
			FilteredRenderable canvas, String valueVar);

	/**
	 * The renderer to use for nodes whose decoration data has been fetched
	 * 
	 * @return
	 */
	protected abstract GeneralRendererSpec getFetchedRenderer(
			FilteredRenderable canvas, String valueVar);

	protected class FetchThread extends Thread {
		protected T result;
		protected IdentifiedObject io;

		public FetchThread(IdentifiedObject io) {
			this.io = io;
		}

		public IdentifiedObject getIdentifiedObject() {
			return io;
		}

		@Override
		public void run() {
			result = getValue(io);
		}

		public T getResult() {
			try {
				join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			return result;
		}
	}

	protected class IsDecoratedCriterion extends AbstractBooleanCriterion {

		public boolean matches(IdentifiedObject o) {
			return decoratedObjects.containsKey(o);
		}

		public String getID() {
			return "is_" + getBehaviorID() + "_fetched_data";
		}
	}

	protected class FetchValueCriterion extends
			AbstractCriterion<IdentifiedObject, T> {

		public String getID() {
			return getBehaviorID() + "_fetch_value";
		}

		public Class<IdentifiedObject> getInputType() {
			return IdentifiedObject.class;
		}

		public Class<T> getReturnType() {
			return valueClass;
		}

		public Collection<T> getValues(Collection<T> scratch,
				IdentifiedObject obj) {
			T val = decoratedObjects.get(obj);
			if (val != null)
				scratch.add(val);
			return scratch;
		}
	}

	protected boolean isFetchCriterionVisibleToOtherComponents() {
		return true;
	}

	protected FetchValueCriterion fetchCriterion = new FetchValueCriterion();
	protected IsDecoratedCriterion isDecoratedCriterion = new IsDecoratedCriterion();

	protected Map<FilteredRenderable, PerPanelTask> taskMap = new HashMap<FilteredRenderable, PerPanelTask>();

	protected GUIComponentListener componentListener = new GUIComponentListener() {

		public void componentHidden(GUIComponentEvent event) {
			if (event.getComponent() instanceof FilteredRenderable) {
				FilteredRenderable canvas = (FilteredRenderable) event
						.getComponent();
				PerPanelTask task = taskMap.get(canvas);
				if (task != null)
					task.shutdown(canvas);

			}
		}

		public void componentShown(GUIComponentEvent event) {
			if (event.getComponent() instanceof FilteredRenderable) {
				FilteredRenderable canvas = (FilteredRenderable) event
						.getComponent();
				PerPanelTask task = new PerPanelTask();
				task.install(canvas);
				taskMap.put(canvas, task);
			}
		}

	};

	public String getValueVarName() {
		return fetchCriterion.getID();
	}

	public void install() {
		ComponentManager.getManager().addComponentListener(componentListener);
		FilterManager.getManager().addCriterion(fetchCriterion,
				isFetchCriterionVisibleToOtherComponents());
		FilterManager.getManager().addCriterion(isDecoratedCriterion,
				isFetchCriterionVisibleToOtherComponents());

	}

	public void shutdown() {
		ComponentManager.getManager()
				.removeComponentListener(componentListener);
		FilterManager.getManager().removeCriterion(fetchCriterion);
		FilterManager.getManager().removeCriterion(isDecoratedCriterion);
	}
}
