package org.oboedit.graph;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.apache.log4j.*;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentWrapper;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.framework.event.GUIComponentEvent;
import org.bbop.framework.event.GUIComponentListener;
import org.bbop.util.ComparableComparator;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.PathCapable;
import org.obo.filters.AbstractBooleanCriterion;
import org.obo.filters.AbstractCriterion;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ExpandCollapseListener;
import org.oboedit.gui.event.ExpansionEvent;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.RenderedFilter;

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

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AbstractFetchTask.class);
	
	public static class FetchTaskConfiguration {
	
		
		protected boolean enabled;

		public FetchTaskConfiguration() {
		}

		public FetchTaskConfiguration(boolean enabled) {
			setEnabled(enabled);
		}

		public boolean isEnabled() {
			return enabled;
		}

		public void setEnabled(boolean enabled) {
			this.enabled = enabled;
		}
	}

	protected class FetchThread extends Thread {
		protected IdentifiedObject io;
		protected T result;

		public FetchThread(IdentifiedObject io) {
			this.io = io;
		}

		public IdentifiedObject getIdentifiedObject() {
			return io;
		}

		public T getResult() {
			boolean running = true;
			while (running) {
				try {
					join();
					running = false;
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			return result;
		}

		@Override
		public void run() {
			result = getValue(io);
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
			try {
				T val = decoratedObjects.get(obj);
				// logger.error("val = " + val
				// + ", decoratedObjects.get("+obj+") = "
				// + decoratedObjects.get(obj) + " decoratedObjects = "
				// + decoratedObjects);
				if (val != null)
					scratch.add(val);
			} catch (Throwable t) {
				t.printStackTrace();
			}
			return scratch;
		}

		@Override
		public String toString() {
			return getID();
		}
	}

	protected class IsDecoratedCriterion extends AbstractBooleanCriterion {

		public String getID() {
			return "is_" + getBehaviorID() + "_fetched_data";
		}

		public boolean matches(IdentifiedObject o) {
			return decoratedObjects.containsKey(o);
		}

		public String toString() {
			return getID();
		}
	}

	protected class PerPanelTask {

		protected class GeneralFetchThread extends Thread {
			protected boolean cancelled = false;

			public void cancel() {
				cancelled = true;
			}

			@Override
			public void interrupt() {
				// TODO Auto-generated method stub
				super.interrupt();
			}

			public void run() {
				while (!cancelled) {
					// Collection<FetchThread> threads = new
					// ArrayList<FetchThread>();
					// while (pendingObjects.size() > 0) {
					// IdentifiedObject io = pendingObjects.removeFirst();
					// decoratedObjects.put(io, getValue(io));
					// logger.error("decoratedObjects =
					// "+decoratedObjects);
					// canvas.relayout();
					// }
					while (pendingObjects.size() > 0) {
						Collection<FetchThread> threads = new ArrayList<FetchThread>();
						while (pendingObjects.size() > 0) {
							IdentifiedObject io = pendingObjects.removeFirst();
							if (!decoratedObjects.containsKey(io)) {
								FetchThread ft = new FetchThread(io);
								threads.add(ft);
								ft.start();
							}
						}
						boolean problem;
						do {
							problem = false;
							for (FetchThread thread : threads) {
								try {
									thread.join();
								} catch (InterruptedException e1) {
									problem = true;
								}
							}
						} while (problem);
						for (FetchThread thread : threads) {
							T result = thread.getResult();
							decoratedObjects.put(thread.getIdentifiedObject(),
									result);
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
					} catch (Throwable t) {
						t.printStackTrace();
					}
				}
			}
		}

		protected FilteredRenderable canvas;
		protected RenderedFilter fetchedRenderFilter;
		protected String id;

		protected ExpandCollapseListener listener = new ExpandCollapseListener() {

			public void expandStateChanged(ExpansionEvent e) {
				pendingObjects.addAll(e.getShown());
				pendingObjects.removeAll(e.getHidden());
				thread.interrupt();
			}

		};

		protected LinkedList<IdentifiedObject> pendingObjects = new LinkedList<IdentifiedObject>();

		protected RenderedFilter pendingRenderFilter;

		protected GeneralFetchThread thread;

		public void queueAll() {
			for (PathCapable pc : canvas.getVisibleObjects()) {
				if (pc instanceof IdentifiedObject) {
					pendingObjects.add((IdentifiedObject) pc);
				}
			}
			thread.interrupt();
		}

		public void queueObjects(Collection<IdentifiedObject> objects) {
			pendingObjects.addAll(objects);
			thread.interrupt();
		}

		public void unqueueObjects(Collection<IdentifiedObject> objects) {
			pendingObjects.removeAll(objects);
			thread.interrupt();
		}

		public void modifyQueue(Collection<IdentifiedObject> add,
				Collection<IdentifiedObject> del) {
			pendingObjects.addAll(add);
			pendingObjects.removeAll(del);
			thread.interrupt();
		}

		public PerPanelTask() {
			id = "" + autogen_id++;
		}

		public void install(FilteredRenderable canvas) {
			this.canvas = canvas;
			canvas.addExpansionListener(listener);
			thread = new GeneralFetchThread();
			thread.start();
			installRenderers();
		}

		public void installRenderers() {
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

		public void uninstallRenderers() {
			if (fetchedRenderFilter != null)
				canvas.removeAutomaticObjectRenderer(fetchedRenderFilter);
			if (pendingRenderFilter != null)
				canvas.removeAutomaticObjectRenderer(pendingRenderFilter);
		}

		public void resetRenderers() {
			uninstallRenderers();
			installRenderers();
		}

		protected boolean isDecorated(IdentifiedObject io) {
			return decoratedObjects.containsKey(io);
		}

		protected boolean isPending(IdentifiedObject io) {
			return pendingObjects.contains(io);
		}

		public void shutdown(FilteredRenderable canvas) {
			thread.cancel();
			canvas.removeExpansionListener(listener);
			uninstallRenderers();
		}

	}

	protected static int autogen_id = 0;

	protected Comparator comparator = ComparableComparator.COMPARATOR;

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
			if (!isEnabled())
				return;
			if (event.getComponent() instanceof FilteredRenderable) {
				FilteredRenderable canvas = (FilteredRenderable) event
						.getComponent();
				PerPanelTask task = new PerPanelTask();
				task.install(canvas);
				taskMap.put(canvas, task);
			}
		}

	};

	protected class ConfigurationComponentFactory extends
			AbstractComponentFactory<GUIComponent> {

		protected JComponent comp;

		public ConfigurationComponentFactory(JComponent comp) {
			this.comp = comp;
		}

		public String getID() {
			return getBehaviorID() + "_component_factory";
		}

		@Override
		public GUIComponent doCreateComponent(String id) {
			return new GUIComponentWrapper(id, getName(), comp);
		}

		public GUIComponentFactory.FactoryCategory getCategory() {
			return FactoryCategory.CONFIG;
		}

		public String getName() {
			return AbstractFetchTask.this.getName() + " Configuration";
		}

		@Override
		public boolean isSingleton() {
			return true;
		}

	}

	private static int idgen = 0;

	protected Map<IdentifiedObject, T> decoratedObjects = new HashMap<IdentifiedObject, T>() {
		int id = idgen++;

		public String toString() {
			return "hashmap" + id + ":" + super.toString();
		}
	};

	protected FetchValueCriterion fetchCriterion = new FetchValueCriterion();

	protected IsDecoratedCriterion isDecoratedCriterion = new IsDecoratedCriterion();

	protected Map<FilteredRenderable, PerPanelTask> taskMap = new HashMap<FilteredRenderable, PerPanelTask>();

	protected Class<T> valueClass;

	protected GUIComponentFactory<GUIComponent> configComponentFactory;

	public AbstractFetchTask(Class<T> valueClass) {
		this.valueClass = valueClass;
	}

	protected boolean enabled = true;

	public void setEnabled(boolean enabled) {
		if (this.enabled != enabled) {
			this.enabled = enabled;
			for (GUIComponent c : ComponentManager.getManager()
					.getActiveComponents()) {
				if (c instanceof FilteredRenderable) {
					FilteredRenderable canvas = (FilteredRenderable) c;
					if (enabled) {
						PerPanelTask task = new PerPanelTask();
						task.install(canvas);
						taskMap.put(canvas, task);
						task.queueAll();
					} else {
						PerPanelTask task = taskMap.get(canvas);
						if (task != null)
							task.shutdown(canvas);
						canvas.redraw();
					}
				}

			}

		}
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void resetRenderers() {
		for (FilteredRenderable fr : taskMap.keySet()) {
			PerPanelTask task = taskMap.get(fr);
			task.resetRenderers();
			fr.redraw();
		}
	}

	public void clearCache() {
		Collection<IdentifiedObject> redrawUs = decoratedObjects.keySet();
		decoratedObjects.clear();
		for (PerPanelTask task : taskMap.values()) {
			task.queueObjects(redrawUs);
		}
	}

	/**
	 * An identifier for this view behavior. This identifier will be used as a
	 * prefix for the identifiers of renderers and search criteria created by
	 * this behavior.
	 * 
	 * @return
	 */
	protected abstract String getBehaviorID();

	/**
	 * The renderer to use for nodes whose decoration data has been fetched
	 * 
	 * @return
	 */
	protected abstract GeneralRendererSpec getFetchedRenderer(
			FilteredRenderable canvas, String valueVar);

	/**
	 * The renderer to use for nodes whose decoration data is still being
	 * fetched
	 * 
	 * @return
	 */
	protected abstract GeneralRendererSpec getPendingRenderer(
			FilteredRenderable canvas, String valueVar);

	/**
	 * Actually returns the value fetched by this view behavior. The graph will
	 * be redrawn when every pending fetch has completed.
	 * 
	 * @param io
	 * @return
	 */
	protected abstract T getValue(IdentifiedObject io);

	protected abstract String getName();

	protected abstract JComponent getConfigurationPanel();

	public String getValueVarName() {
		return fetchCriterion.getID();
	}

	public void setConfiguration(Object config) {
		if (config instanceof FetchTaskConfiguration) {
			setEnabled(((FetchTaskConfiguration) config).isEnabled());
		}
	}

	public Object getConfiguration() {
		return new FetchTaskConfiguration();
	}

	public void install() {
		ComponentManager.getManager().addComponentListener(componentListener);
		FilterManager.getManager().addCriterion(fetchCriterion,
				isFetchCriterionVisibleToOtherComponents());
		FilterManager.getManager().addCriterion(isDecoratedCriterion,
				isFetchCriterionVisibleToOtherComponents());
		JComponent comp = getConfigurationPanel();
		if (comp != null) {
			configComponentFactory = new ConfigurationComponentFactory(comp);
			ComponentManager.getManager().install(configComponentFactory);
		}
		readConfig();

	}

	protected void readConfig() {
		Object config = getConfiguration();
		File f = getConfigFile();
		if (f.exists()) {
			try {
				XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(
						new FileInputStream(f)));
				config = decoder.readObject();
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		setConfiguration(config);
	}

	protected void flushConfig() {
		File f = getConfigFile();
		try {
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(f)));
			encoder.writeObject(getConfiguration());
			encoder.close();
		} catch (IOException ex) {
			logger.error("Couldn't flush component config successfully");
		}
	}

	protected File getConfigFile() {
		return new File(GUIManager.getPrefsDir(), getBehaviorID()
				+ "_behavior.config");
	}

	protected boolean isFetchCriterionVisibleToOtherComponents() {
		return false;
	}

	public void shutdown() {
		flushConfig();
		ComponentManager.getManager()
				.removeComponentListener(componentListener);
		FilterManager.getManager().removeCriterion(fetchCriterion);
		FilterManager.getManager().removeCriterion(isDecoratedCriterion);
		if (configComponentFactory != null) {
			ComponentManager.getManager().uninstall(configComponentFactory);
		}
	}
}
