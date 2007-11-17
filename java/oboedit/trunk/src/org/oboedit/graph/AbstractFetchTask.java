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
import org.oboedit.gui.components.GraphEditor;
import org.oboedit.gui.components.LinkDatabaseCanvas;
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
 * {@link #getFetchedRenderer()} will be used to render the new nodes, and the
 * methods {@link #decoratePending(OENode)} and
 * {@link #decorateComplete(OENode, Object)} can be used to further decorate the
 * nodes.
 * 
 * @author jrichter
 * 
 * @param <T>
 *            The datatype of the value fetched by
 *            {@link #getValue(IdentifiedObject)}
 */
public abstract class AbstractFetchTask<T> implements ViewBehavior,
		NodeDecorator, GUITask {

	protected ExpandCollapseListener listener = new ExpandCollapseListener() {

		public void expandStateChanged(ExpansionEvent e) {
			pendingObjects.addAll(e.getShown());
			decoratedObjects.keySet().removeAll(e.getHidden());
			pendingObjects.removeAll(e.getHidden());
			thread.interrupt();
		}

	};

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

	protected class MaxCriterion extends AbstractCriterion<OBOSession, T> {

		public String getID() {
			// TODO Auto-generated method stub
			return getBehaviorID() + "_max_fetch_value";
		}

		public Class<OBOSession> getInputType() {
			return OBOSession.class;
		}

		public Class<T> getReturnType() {
			return valueClass;
		}

		public Collection<T> getValues(Collection<T> scratch, OBOSession obj) {
			T val = getMaxValue();
			if (val != null)
				scratch.add(val);
			return scratch;
		}

	}

	protected class MinCriterion extends AbstractCriterion<OBOSession, T> {

		public String getID() {
			// TODO Auto-generated method stub
			return getBehaviorID() + "_min_fetch_value";
		}

		public Class<OBOSession> getInputType() {
			return OBOSession.class;
		}

		public Class<T> getReturnType() {
			return valueClass;
		}

		public Collection<T> getValues(Collection<T> scratch, OBOSession obj) {
			T val = getMinValue();
			if (val != null)
				scratch.add(val);
			return scratch;
		}

	}

	protected Comparator comparator = ComparableComparator.COMPARATOR;

	protected LinkedList<IdentifiedObject> pendingObjects = new LinkedList<IdentifiedObject>();

	protected Map<IdentifiedObject, T> decoratedObjects = new HashMap<IdentifiedObject, T>();

	protected LinkDatabaseCanvas canvas;

	protected boolean cancelled = false;

	protected Class<T> valueClass;

	protected FetchValueCriterion fetchCriterion = new FetchValueCriterion();
	protected MaxCriterion maxCriterion = new MaxCriterion();
	protected MinCriterion minCriterion = new MinCriterion();
	protected IsDecoratedCriterion isDecoratedCriterion = new IsDecoratedCriterion();

	public AbstractFetchTask(Class<T> valueClass) {
		this.valueClass = valueClass;
	}

	protected class GeneralFetchThread extends Thread {
		public void run() {
			while (!cancelled) {
				// Collection<FetchThread> threads = new
				// ArrayList<FetchThread>();
				// while (pendingObjects.size() > 0) {
				// IdentifiedObject io = pendingObjects.removeFirst();
				// decoratedObjects.put(io, getValue(io));
				// System.err.println("decoratedObjects = "+decoratedObjects);
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
						canvas.relayout();
					}
				});
				try {
					join();
				} catch (InterruptedException e) {
				}
			}
		}
	}

	protected GeneralFetchThread thread;

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
	 * Called to decorate a node whose decoration data is still being fetched.
	 * This should only be used to apply decorations that cannot be attached via
	 * a renderer (like heat map colorings)
	 * 
	 * @param node
	 */
	protected abstract void decoratePending(OENode node);

	/**
	 * Called to decorate a node whose decoration data has been fetched. This
	 * should only be used to apply decorations that cannot be attached via a
	 * renderer (like heat map colorings)
	 * 
	 * @param node
	 * @param value
	 *            The value fetched by getValue()
	 */
	protected abstract void decorateComplete(OENode node, T value);

	/**
	 * The renderer to use for nodes whose decoration data is still being
	 * fetched
	 * 
	 * @return
	 */
	protected abstract GeneralRendererSpec getPendingRenderer();

	/**
	 * The renderer to use for nodes whose decoration data has been fetched
	 * 
	 * @return
	 */
	protected abstract GeneralRendererSpec getFetchedRenderer();

	public T getMaxValue() {
		T max = null;
		for (T val : decoratedObjects.values()) {
			if (max == null || comparator.compare(val, max) > 0)
				max = val;
		}
		return max;
	}

	public T getMinValue() {
		T min = null;
		for (T val : decoratedObjects.values()) {
			if (min == null || comparator.compare(val, min) < 0)
				min = val;
		}
		return min;
	}

	public static double getRatio(Number val, Number min, Number max) {
		if (max == null)
			max = Double.MAX_VALUE;
		if (min == null)
			min = new Double(0);
		return getRatio(val.doubleValue(), min.doubleValue(), max.doubleValue());
	}

	public static double getRatio(double val, double min, double max) {
		return (val - min) / (max - min);
	}

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

	public PActivity decorate(PNode node, boolean noAnimation) {
		if (node instanceof OENode) {
			OENode oenode = (OENode) node;
			if (isPending((IdentifiedObject) oenode.getObject()))
				decoratePending(oenode);
			else if (isDecorated((IdentifiedObject) oenode.getObject()))
				decorateComplete(oenode, decoratedObjects
						.get((IdentifiedObject) oenode.getObject()));
		}
		return null;
	}

	public boolean onlyDecorateAfterLayout() {
		return true;
	}

	protected boolean isPending(IdentifiedObject io) {
		return pendingObjects.contains(io);
	}

	protected boolean isDecorated(IdentifiedObject io) {
		return decoratedObjects.containsKey(io);
	}

	public void cancel() {
		cancelled = true;
	}

	protected RenderedFilter fetchedRenderFilter;
	protected RenderedFilter pendingRenderFilter;

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addExpansionListener(listener);
		thread = new GeneralFetchThread();
		thread.start();
		if (getFetchedRenderer() != null) {
			ObjectFilter filter = new ObjectFilterImpl();
			filter.setCriterion(isDecoratedCriterion);
			fetchedRenderFilter = new RenderedFilter(filter,
					getFetchedRenderer());
			canvas.addAutomaticObjectRenderer(fetchedRenderFilter);
		}
		if (getPendingRenderer() != null) {
			ObjectFilter filter = new ObjectFilterImpl();
			filter.setNegate(true);
			filter.setCriterion(isDecoratedCriterion);
			pendingRenderFilter = new RenderedFilter(filter,
					getPendingRenderer());
			canvas.addAutomaticObjectRenderer(pendingRenderFilter);
		}
		canvas.addDecorator(this);
	}

	public String getValueVarName() {
		return fetchCriterion.getID();
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		cancel();
		canvas.removeExpansionListener(listener);
		if (fetchedRenderFilter != null)
			canvas.removeAutomaticObjectRenderer(fetchedRenderFilter);
		if (pendingRenderFilter != null)
			canvas.removeAutomaticObjectRenderer(pendingRenderFilter);
		canvas.removeDecorator(this);
	}

	protected GUIComponentListener componentListener = new GUIComponentListener() {

		public void componentHidden(GUIComponentEvent event) {
			if (event.getComponent() instanceof LinkDatabaseCanvas) {
				LinkDatabaseCanvas canvas = (LinkDatabaseCanvas) event
						.getComponent();
				canvas.removeViewBehavior(AbstractFetchTask.this);
			}
		}

		public void componentShown(GUIComponentEvent event) {
			if (event.getComponent() instanceof LinkDatabaseCanvas) {
				LinkDatabaseCanvas canvas = (LinkDatabaseCanvas) event
						.getComponent();
				canvas.addViewBehavior(AbstractFetchTask.this);
			}
		}

	};

	public void install() {
		ComponentManager.getManager().addComponentListener(componentListener);
		FilterManager.getManager().addCriterion(fetchCriterion);
		FilterManager.getManager().addCriterion(maxCriterion);
		FilterManager.getManager().addCriterion(minCriterion);
		FilterManager.getManager().addCriterion(isDecoratedCriterion);
	}

	public void shutdown() {
		ComponentManager.getManager()
				.removeComponentListener(componentListener);
		FilterManager.getManager().removeCriterion(fetchCriterion);
		FilterManager.getManager().removeCriterion(maxCriterion);
		FilterManager.getManager().removeCriterion(minCriterion);
		FilterManager.getManager().removeCriterion(isDecoratedCriterion);
	}
}
