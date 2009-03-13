package org.oboedit.controller;

import java.awt.Color;
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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.framework.GUIManager;
import org.obo.filters.AllTextFieldsCriterion;
import org.obo.filters.AncestorSearchAspect;
import org.obo.filters.CategorySearchCriterion;
import org.obo.filters.ChildSearchAspect;
import org.obo.filters.CommentSearchCriterion;
import org.obo.filters.CompoundFilter;
import org.obo.filters.CompoundFilterImpl;
import org.obo.filters.ContainsComparison;
import org.obo.filters.DbxrefSearchCriterion;
import org.obo.filters.DefinitionDbxrefSearchCriterion;
import org.obo.filters.DefinitionSearchCriterion;
import org.obo.filters.DescendantSearchAspect;
import org.obo.filters.EndsWithComparison;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.GeneralDbxrefSearchCriterion;
import org.obo.filters.GreaterThanComparison;
import org.obo.filters.GreaterThanEqualsComparison;
import org.obo.filters.HasIsaParentCriterion;
import org.obo.filters.HasParentWithTypeIDCriterion;
import org.obo.filters.IDSearchCriterion;
import org.obo.filters.IsAnonymousCriterion;
import org.obo.filters.IsBuiltinCriterion;
import org.obo.filters.IsClassCriterion;
import org.obo.filters.IsCompleteCriterion;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsImpliedLinkCriterion;
import org.obo.filters.IsNecessaryCriterion;
import org.obo.filters.IsObsoleteCriterion;
import org.obo.filters.IsPropertyCriterion;
import org.obo.filters.IsRedundantLinkCriterion;
import org.obo.filters.IsTransitiveCriterion;
import org.obo.filters.IsaCompleteCriterion;
import org.obo.filters.IsaParentCountCriterion;
import org.obo.filters.KeywordSearchCriterion;
import org.obo.filters.LessThanComparison;
import org.obo.filters.LessThanEqualsComparison;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterImpl;
import org.obo.filters.LinkNamespaceSearchCriterion;
import org.obo.filters.MultipleRootSearchCriterion;
import org.obo.filters.NameSearchCriterion;
import org.obo.filters.NameSynonymSearchCriterion;
import org.obo.filters.NamespaceSearchCriterion;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.obo.filters.ParentCountCriterion;
import org.obo.filters.ParentSearchAspect;
import org.obo.filters.ParentSearchCriterion;
import org.obo.filters.RegexpComparison;
import org.obo.filters.RootSearchAspect;
import org.obo.filters.SearchAspect;
import org.obo.filters.SearchComparison;
import org.obo.filters.SearchCriterion;
import org.obo.filters.SelfSearchAspect;
import org.obo.filters.StartsWithComparison;
import org.obo.filters.SynonymDbxrefSearchCriterion;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.filters.WildcardComparison;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerListener;
import org.obo.util.FilterUtil;
import org.oboedit.gui.LineType;
import org.oboedit.gui.event.GlobalFilterListener;
import org.oboedit.gui.event.ReasonerStatusEvent;
import org.oboedit.gui.event.ReasonerStatusListener;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.BoldSpecField;
import org.oboedit.gui.filter.ConfiguredColor;
import org.oboedit.gui.filter.FontFaceSpecField;
import org.oboedit.gui.filter.FontSizeSpecField;
import org.oboedit.gui.filter.ForegroundColorSpecField;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.GeneralRendererSpecField;
import org.oboedit.gui.filter.HTMLSpecField;
import org.oboedit.gui.filter.ItalicSpecField;
import org.oboedit.gui.filter.LineTypeSpecField;
import org.oboedit.gui.filter.LineWidthSpecField;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.gui.filter.StrikeoutSpecField;
import org.oboedit.gui.filter.UnderlineSpecField;

import org.apache.log4j.*;

public class FilterManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FilterManager.class);

	protected static FilterManager manager;

	protected Map<String, SearchCriterion> criteria = new LinkedHashMap<String, SearchCriterion>();
	protected List<SearchAspect> aspects = new LinkedList<SearchAspect>();
	protected List<SearchComparison> comparisons = new LinkedList<SearchComparison>();

	protected Filter globalLinkFilter;
	protected Collection<RenderedFilter> globalLinkRenderers;
	protected Filter globalTermFilter;
	protected Collection<RenderedFilter> globalTermRenderers;

	protected Collection<GlobalFilterListener> globalFilterListeners = new LinkedList<GlobalFilterListener>();

	protected Collection<GeneralRendererSpecField<?>> renderSpecFields = new ArrayList<GeneralRendererSpecField<?>>();

	protected List<SearchCriterion> displayableCriteria = new ArrayList<SearchCriterion>();

	public void addGlobalFilterListener(GlobalFilterListener listener) {
		globalFilterListeners.add(listener);
	}

	public void removeGlobalFilterListener(GlobalFilterListener listener) {
		globalFilterListeners.add(listener);
	}

	protected void fireGlobalFilterChange() {
		for (GlobalFilterListener listener : globalFilterListeners) {
			listener.globalFilterChanged();
		}
	}

	private FilterManager() {
		installDefaults();
		SessionManager.getManager().addReasonerListener(new ReasonerListener() {

			public void reasoningFinished() {
				updateReasoner();
			}
			
			public void reasoningCancelled() {
				updateReasoner();
			}

			public void reasoningStarted() {
			}

		}, true);
		SessionManager.getManager().addReasonerStatusListener(
				new ReasonerStatusListener() {

					public void statusChanged(ReasonerStatusEvent e) {
						updateReasoner();
					}

				});
		updateReasoner();
	}

	protected void updateReasoner() {
		ReasonedLinkDatabase r = SessionManager.getManager().getReasoner();
		for (SearchCriterion<?, ?> crit : getCriteria()) {
			crit.setReasoner(r);
		}
	}

	public static FilterManager getManager() {
		if (manager == null)
			manager = createManager();
		return manager;
	}

	protected void installDefaults() {
		addComparison(new ContainsComparison());
		addComparison(new EqualsComparison());
		addComparison(new StartsWithComparison());
		addComparison(new EndsWithComparison());
		addComparison(new WildcardComparison());
		addComparison(new RegexpComparison());
		addComparison(new LessThanComparison());
		addComparison(new LessThanEqualsComparison());
		addComparison(new GreaterThanComparison());
		addComparison(new GreaterThanEqualsComparison());

		addCriterion(new AllTextFieldsCriterion());
		addCriterion(new NameSynonymSearchCriterion());
		addCriterion(new IDSearchCriterion());
		addCriterion(new CategorySearchCriterion());
		addCriterion(new NameSearchCriterion());
		addCriterion(new SynonymSearchCriterion());
		addCriterion(new CommentSearchCriterion());
		addCriterion(new DefinitionSearchCriterion());
		addCriterion(new NamespaceSearchCriterion());
		addCriterion(new LinkNamespaceSearchCriterion());
		addCriterion(new DbxrefSearchCriterion());
		addCriterion(new GeneralDbxrefSearchCriterion());
		addCriterion(new DefinitionDbxrefSearchCriterion());
		addCriterion(new SynonymDbxrefSearchCriterion());
		addCriterion(new IsCompleteCriterion());
		addCriterion(new IsaCompleteCriterion());
		addCriterion(new HasIsaParentCriterion());
		addCriterion(new ParentSearchCriterion(), false);
		addCriterion(new HasParentWithTypeIDCriterion());
		addCriterion(new IsAnonymousCriterion());
		addCriterion(new IsCompleteLinkCriterion());
		addCriterion(new IsClassCriterion());
		addCriterion(new IsObsoleteCriterion());
		addCriterion(new IsPropertyCriterion());
		addCriterion(new IsNecessaryCriterion());
		addCriterion(new IsaParentCountCriterion());
		addCriterion(new ParentCountCriterion());
		addCriterion(new IsImpliedLinkCriterion());
		addCriterion(new IsRedundantLinkCriterion());
		addCriterion(new IsTransitiveCriterion());
		addCriterion(new IsBuiltinCriterion());
		addCriterion(new KeywordSearchCriterion());
		addCriterion(new MultipleRootSearchCriterion());

		addAspect(new SelfSearchAspect());
		addAspect(new RootSearchAspect());
		addAspect(new AncestorSearchAspect());
		addAspect(new DescendantSearchAspect());
		addAspect(new ParentSearchAspect());
		addAspect(new ChildSearchAspect());

		buildDefaultGlobalTermFilter();
		buildDefaultGlobalLinkFilter();

		buildDefaultGlobalTermRenderers();
		buildDefaultGlobalLinkRenderers();

		addRenderSpecField(ForegroundColorSpecField.FIELD);
		addRenderSpecField(LineWidthSpecField.FIELD);
		addRenderSpecField(LineTypeSpecField.FIELD);
		addRenderSpecField(BackgroundColorSpecField.FIELD);
		addRenderSpecField(FontFaceSpecField.FIELD);
		addRenderSpecField(FontSizeSpecField.FIELD);
		addRenderSpecField(BoldSpecField.FIELD);
		addRenderSpecField(ItalicSpecField.FIELD);
		addRenderSpecField(UnderlineSpecField.FIELD);
		addRenderSpecField(StrikeoutSpecField.FIELD);
		addRenderSpecField(HTMLSpecField.FIELD);
	}

	protected static FilterManager createManager() {
		final FilterManager manager = new FilterManager();
		manager.readConfig();
		GUIManager.addShutdownHook(new Runnable() {

			public void run() {
				manager.flushConfig();
			}
		});
		return manager;
	}

	@SuppressWarnings("unchecked")
	public Filter<?> getAugmentedLinkFilter(Filter linkFilter) {
//		logger.debug("FilterManager.getAugmentedLinkFilter: merging supplied linkFilter " + linkFilter + " with global filter " + getGlobalLinkFilter()); // DEL
		return FilterUtil.mergeFilters(linkFilter, getGlobalLinkFilter());
	}

	@SuppressWarnings("unchecked")
	public Filter<?> getAugmentedTermFilter(Filter termFilter) {
		return FilterUtil.mergeFilters(termFilter, getGlobalTermFilter());
	}

	public void addCriterion(SearchCriterion c) {
		addCriterion(c, true);
	}

	public void addCriterion(SearchCriterion c, boolean displayable) {
		criteria.put(c.getID(), c);
		if (displayable)
			displayableCriteria.add(c);
	}

	public void removeCriterion(SearchCriterion c) {
		criteria.remove(c.getID());
		displayableCriteria.remove(c);
	}

	public void addAspect(SearchAspect aspect) {
		aspects.add(aspect);
	}

	public void removeAspect(SearchAspect aspect) {
		aspects.remove(aspect);
	}

	public void addComparison(SearchComparison comparison) {
		comparisons.add(comparison);
	}

	public void removeComparison(SearchComparison comparison) {
		comparisons.remove(comparison);
	}

	public Collection<SearchComparison> getComparisons() {
		return comparisons;
	}

	public Collection<SearchAspect> getAspects() {
		return aspects;
	}

	public Collection<SearchCriterion> getCriteria() {
		return criteria.values();
	}

	public Collection<SearchCriterion> getDisplayableCriteria() {
		return displayableCriteria;
	}

	public SearchCriterion getCriterion(String id) {
		return criteria.get(id);
	}

	protected void buildDefaultGlobalLinkFilter() {
		// Default filter:  is_intersection NOT
		ObjectFilter iclfilter = new ObjectFilterImpl();
		iclfilter.setNegate(true);
		iclfilter.setCriterion(new IsCompleteLinkCriterion());

		// 3/24/08: Don't (by default) filter out links that aren't transitive.  That excludes the regulates links.
//		ObjectFilter istransitiveobjectfilter = new ObjectFilterImpl();
//		istransitiveobjectfilter.setCriterion(new IsTransitiveCriterion());

		LinkFilter completeFilter = new LinkFilterImpl();
		completeFilter.setAspect(LinkFilter.SELF);
		completeFilter.setFilter(iclfilter);

//		LinkFilter transitiveFilter = new LinkFilterImpl();
//		transitiveFilter.setAspect(LinkFilter.TYPE);
//		transitiveFilter.setFilter(istransitiveobjectfilter);

//		CompoundFilter andCompleteFilter = new CompoundFilterImpl();
//		andCompleteFilter.addFilter(completeFilter);
//		andCompleteFilter.addFilter(transitiveFilter);

//		setGlobalLinkFilter(andCompleteFilter);
		setGlobalLinkFilter(completeFilter);
	}

	protected void buildDefaultGlobalLinkRenderers() {
		globalLinkRenderers = new LinkedList<RenderedFilter>();
		globalLinkRenderers.add(createRedundantRenderer());
		globalLinkRenderers.add(createImpliedRenderer());
	}

	protected void buildDefaultGlobalTermFilter() {
		globalTermFilter = new CompoundFilterImpl(CompoundFilter.AND);
		ObjectFilter propCriterion = new ObjectFilterImpl();
		propCriterion.setNegate(false);
		propCriterion.setCriterion(new IsPropertyCriterion());

		ObjectFilter builtInCriterion = new ObjectFilterImpl();
		builtInCriterion.setNegate(false);
		builtInCriterion.setCriterion(new IsBuiltinCriterion());

		CompoundFilter builtInProps = new CompoundFilterImpl();
		builtInProps.addFilter(propCriterion);
		builtInProps.addFilter(builtInCriterion);

		ObjectFilter nonBuiltInObjects = new ObjectFilterImpl();
		nonBuiltInObjects.setNegate(true);
		nonBuiltInObjects.setCriterion(new IsBuiltinCriterion());

		CompoundFilter builtInClassesFilter2 = new CompoundFilterImpl(
				CompoundFilter.OR);
		builtInClassesFilter2.addFilter(builtInProps);
		builtInClassesFilter2.addFilter(nonBuiltInObjects);

		setGlobalTermFilter(builtInClassesFilter2);
	}

	protected void buildDefaultGlobalTermRenderers() {
		globalTermRenderers = new LinkedList<RenderedFilter>();
		globalTermRenderers.add(createObsoleteRenderer());
		globalTermRenderers.add(createPropertyRenderer());
	}

	protected RenderedFilter createImpliedRenderer() {
		ObjectFilter typeFilter = new ObjectFilterImpl();
		typeFilter.setCriterion(new IsImpliedLinkCriterion());
		org.obo.filters.LinkFilter basicLinkFilter = new LinkFilterImpl();
		basicLinkFilter.setAspect(org.obo.filters.LinkFilter.SELF);
		basicLinkFilter.setFilter(typeFilter);

		GeneralRendererSpec spec = new GeneralRendererSpec(
				LineTypeSpecField.FIELD, LineType.DASHED_LINE);

		return new RenderedFilter(basicLinkFilter, spec);
	}

	protected RenderedFilter createObsoleteRenderer() {
		ObjectFilter filter = new ObjectFilterImpl();
		filter.setCriterion(new IsObsoleteCriterion());

		GeneralRendererSpec spec = new GeneralRendererSpec();
		spec.setValue(ForegroundColorSpecField.FIELD, new ConfiguredColor(
				Color.red, true));
		spec.setValue(StrikeoutSpecField.FIELD, true);
		return new RenderedFilter(filter, spec);
	}

	protected RenderedFilter createPropertyRenderer() {
		ObjectFilter filter = new ObjectFilterImpl();
		filter.setCriterion(new IsPropertyCriterion());

		GeneralRendererSpec spec = new GeneralRendererSpec();
		spec.setValue(ForegroundColorSpecField.FIELD, new ConfiguredColor(
				Color.blue, true));
		return new RenderedFilter(filter, spec);
	}

	protected RenderedFilter createRedundantRenderer() {
		ObjectFilter typeFilter = new ObjectFilterImpl();
		typeFilter.setCriterion(IsRedundantLinkCriterion.CRITERION);
		org.obo.filters.LinkFilter basicLinkFilter = new LinkFilterImpl();
		basicLinkFilter.setAspect(org.obo.filters.LinkFilter.SELF);
		basicLinkFilter.setFilter(typeFilter);

		GeneralRendererSpec spec = new GeneralRendererSpec(
				LineTypeSpecField.FIELD, LineType.ZIGZAG_LINE,
				ForegroundColorSpecField.FIELD, new ConfiguredColor(Color.red,
						true));
		return new RenderedFilter(basicLinkFilter, spec);
	}

	public void setGlobalFilters(Filter objectFilter, Filter linkFilter) {
		setGlobalLinkFilter(linkFilter);
		this.globalTermFilter = objectFilter;
		fireGlobalFilterChange();
	}

	protected void setGlobalLinkFilter(Filter linkFilter) {
//		logger.debug("FilterManager.setGlobalLinkFilter(" + linkFilter + ")"); // DEL
		this.globalLinkFilter = linkFilter;
	}

	public Filter<?> getGlobalLinkFilter() {
		return globalLinkFilter;
	}

	public Collection<RenderedFilter> getGlobalLinkRenderers() {
		return globalLinkRenderers;
	}

	public Filter<?> getGlobalTermFilter() {
		return globalTermFilter;
	}

	public Collection<RenderedFilter> getGlobalTermRenderers() {
		return globalTermRenderers;
	}

	public void setGlobalLinkRenderers(List<RenderedFilter> globalLinkRenderers) {
		this.globalLinkRenderers = globalLinkRenderers;
	}

	public void setGlobalTermRenderers(List<RenderedFilter> globalTermRenderers) {
		this.globalTermRenderers = globalTermRenderers;
	}

	protected void flushConfig() {
		try {
			// use a safe file writer here eventually
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(
					new FileOutputStream(new File(GUIManager.getPrefsDir(),
					"filter_prefs.xml"))));
			Map config = getGlobalFilterConfig();
			encoder.writeObject(config);
			encoder.close();
		} catch (IOException ex) {
			logger.error("Couldn't flush component config successfully");
		}
	}

	@SuppressWarnings("unchecked")
	protected Map<String, Object> getGlobalFilterConfig() {
		Map<String, Object> out = new HashMap<String, Object>();

		out.put("Controller.globalLinkFilter", getGlobalLinkFilter());
		out.put("Controller.globalTermFilter", getGlobalTermFilter());
		out.put("Controller.globalTermRenderers", getGlobalTermRenderers());
		out.put("Controller.globalLinkRenderers", getGlobalLinkRenderers());
		return out;
	}

	@SuppressWarnings("unchecked")
	protected Map<String, Collection> getSavedComponentConfigMap() {
		try {
			XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(
					new FileInputStream(new File(GUIManager.getPrefsDir(),
					"filter_prefs.xml"))));
			Map<String, Collection> out = (Map) decoder.readObject();
			decoder.close();
			return out;
		} catch (Exception ex) {
			return new HashMap<String, Collection>();
		}
	}

	protected void readConfig() {
		Map<String, Collection> configMap = getSavedComponentConfigMap();

		setGlobalTermFilter((Filter) configMap
				.get("Controller.globalTermFilter"));
		setGlobalLinkFilter((Filter) configMap
				.get("Controller.globalLinkFilter"));

		setGlobalTermRenderers((Collection) configMap
				.get("Controller.globalTermRenderers"));

		setGlobalLinkRenderers((Collection) configMap
				.get("Controller.globalLinkRenderers"));

		if (globalTermFilter == null)
			buildDefaultGlobalTermFilter();
		if (globalLinkFilter == null)
			buildDefaultGlobalLinkFilter();

		if (globalTermRenderers == null)
			buildDefaultGlobalTermRenderers();
		if (globalLinkRenderers == null)
			buildDefaultGlobalLinkRenderers();
	}

	protected void setGlobalLinkRenderers(Collection<RenderedFilter> collection) {
		globalLinkRenderers = collection;
	}

	protected void setGlobalTermRenderers(Collection<RenderedFilter> collection) {
		globalTermRenderers = collection;
	}

	protected void setGlobalTermFilter(Filter filter) {
		this.globalTermFilter = filter;
	}

	public Collection<GeneralRendererSpecField<?>> getRenderSpecFields() {
		return renderSpecFields;
	}

	public void addRenderSpecField(GeneralRendererSpecField<?> field) {
		renderSpecFields.add(field);
	}

	public void removeRenderSpecField(GeneralRendererSpecField<?> field) {
		renderSpecFields.remove(field);
	}
}
