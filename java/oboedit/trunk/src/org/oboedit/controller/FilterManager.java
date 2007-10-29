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
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.framework.GUIManager;
import org.obo.filters.AllTextFieldsCriterion;
import org.obo.filters.AncestorSearchAspect;
import org.obo.filters.CategorySearchCriterion;
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
import org.obo.filters.FilterPair;
import org.obo.filters.FilterPairImpl;
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
import org.obo.filters.IsImpliedObjectCriterion;
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
import org.obo.filters.LinkRenderSpec;
import org.obo.filters.NameSearchCriterion;
import org.obo.filters.NameSynonymSearchCriterion;
import org.obo.filters.NamespaceSearchCriterion;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.obo.filters.ObjectRenderSpec;
import org.obo.filters.ParentCountCriterion;
import org.obo.filters.RegexpComparison;
import org.obo.filters.RenderedFilter;
import org.obo.filters.RootSearchAspect;
import org.obo.filters.SearchAspect;
import org.obo.filters.SearchComparison;
import org.obo.filters.SearchCriterion;
import org.obo.filters.SelfSearchAspect;
import org.obo.filters.StartsWithComparison;
import org.obo.filters.SynonymDbxrefSearchCriterion;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.filters.WildcardComparison;
import org.obo.reasoner.ReasonerListener;
import org.obo.util.FilterUtil;
import org.oboedit.gui.event.GlobalFilterListener;

public class FilterManager {

	protected static FilterManager manager;

	protected List<SearchCriterion> criteria = new LinkedList<SearchCriterion>();
	protected List<SearchAspect> aspects = new LinkedList<SearchAspect>();
	protected List<SearchComparison> comparisons = new LinkedList<SearchComparison>();

	protected Filter globalLinkFilter;
	protected Collection<RenderedFilter> globalLinkRenderers;
	protected Filter globalTermFilter;
	protected Collection<RenderedFilter> globalTermRenderers;

	protected Collection<GlobalFilterListener> globalFilterListeners = new LinkedList<GlobalFilterListener>();
	protected List<FilterPair> modifyFilters = new LinkedList<FilterPair>();

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
		addCriterion(new IsImpliedObjectCriterion());
		final IsRedundantLinkCriterion c = new IsRedundantLinkCriterion();
		SessionManager.getManager().addReasonerListener(new ReasonerListener() {

			public void reasoningFinished() {
				c.setReasoner(SessionManager.getManager().getReasoner());
			}

			public void reasoningStarted() {
			}

		}, true);
		addCriterion(c);
		addCriterion(new IsTransitiveCriterion());
		addCriterion(new IsBuiltinCriterion());
		addCriterion(new KeywordSearchCriterion());

		addAspect(new SelfSearchAspect());
		addAspect(new RootSearchAspect());
		addAspect(new AncestorSearchAspect());
		addAspect(new DescendantSearchAspect());

		buildDefaultGlobalTermFilter();
		buildDefaultGlobalLinkFilter();

		buildDefaultGlobalTermRenderers();
		buildDefaultGlobalLinkRenderers();
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
		return FilterUtil.mergeFilters(linkFilter, getGlobalLinkFilter());
	}
	
	@SuppressWarnings("unchecked")
	public Filter<?> getAugmentedTermFilter(Filter termFilter) {
		return FilterUtil.mergeFilters(termFilter, getGlobalTermFilter());
	}

	public void addCriterion(SearchCriterion c) {
		criteria.add(c);
	}

	public void removeCriterion(SearchCriterion c) {
		criteria.remove(c);
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
		return criteria;
	}

	protected void buildDefaultGlobalLinkFilter() {
		// globalLinkFilter = new CompoundFilterImpl(CompoundFilter.AND);
		ObjectFilter iclfilter = new ObjectFilterImpl();
		iclfilter.setNegate(true);
		iclfilter.setCriterion(new IsCompleteLinkCriterion());

		ObjectFilter istransitiveobjectfilter = new ObjectFilterImpl();
		istransitiveobjectfilter.setCriterion(new IsTransitiveCriterion());

		LinkFilter completeFilter = new LinkFilterImpl();
		completeFilter.setAspect(LinkFilter.SELF);
		completeFilter.setFilter(iclfilter);

		LinkFilter transitiveFilter = new LinkFilterImpl();
		transitiveFilter.setAspect(LinkFilter.TYPE);
		transitiveFilter.setFilter(istransitiveobjectfilter);

		CompoundFilter andCompleteFilter = new CompoundFilterImpl();
		andCompleteFilter.addFilter(completeFilter);
		andCompleteFilter.addFilter(transitiveFilter);

		setGlobalLinkFilter(andCompleteFilter);
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
		basicLinkFilter
				.setAspect(org.obo.filters.LinkFilter.SELF);
		basicLinkFilter.setFilter(typeFilter);

		LinkRenderSpec spec = new LinkRenderSpec();
		spec.setLinkColor(Color.blue);
		spec.setLineType(LinkRenderSpec.WAVY_LINE);
		spec.setLineWidth(2);

		return new RenderedFilter(basicLinkFilter, spec);
	}

	protected RenderedFilter createObsoleteRenderer() {
		ObjectFilter filter = new ObjectFilterImpl();
		filter.setCriterion(new IsObsoleteCriterion());

		ObjectRenderSpec spec = new ObjectRenderSpec();
		spec.setForegroundColor(Color.red);
		return new RenderedFilter(filter, spec);
	}

	protected RenderedFilter createPropertyRenderer() {
		ObjectFilter filter = new ObjectFilterImpl();
		filter.setCriterion(new IsPropertyCriterion());

		ObjectRenderSpec spec = new ObjectRenderSpec();
		spec.setForegroundColor(Color.blue);
		return new RenderedFilter(filter, spec);
	}

	protected RenderedFilter createRedundantRenderer() {
		ObjectFilter typeFilter = new ObjectFilterImpl();
		typeFilter.setCriterion(new IsRedundantLinkCriterion());
		org.obo.filters.LinkFilter basicLinkFilter = new LinkFilterImpl();
		basicLinkFilter
				.setAspect(org.obo.filters.LinkFilter.SELF);
		basicLinkFilter.setFilter(typeFilter);

		LinkRenderSpec spec = new LinkRenderSpec();
		spec.setLinkColor(Color.red);
		spec.setLineWidth(2);
		return new RenderedFilter(basicLinkFilter, spec);
	}

	public void setGlobalFilters(Filter objectFilter, Filter linkFilter) {
		setGlobalLinkFilter(linkFilter);
		this.globalTermFilter = objectFilter;
		fireGlobalFilterChange();
	}

	protected void setGlobalLinkFilter(Filter linkFilter) {
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

	public void addModifyFilter(FilterPair filterPair) {
		modifyFilters.add(filterPair);
	}

	public List<FilterPair> getModifyFilters() {
		return modifyFilters;
	}

	public void removeModifyFilter(FilterPair filterPair) {
		modifyFilters.remove(filterPair);
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
			System.err.println("Couldn't flush component config successfully");
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
}
