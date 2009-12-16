package org.oboedit.gui.components.ontologyGeneration;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Internal representation of a definition
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class CandidateDefinition implements Cloneable
{
	public static final String PROPERTY_DEFINITION = "definition";
	public static final String PROPERTY_SELECTION = "selection";
	public static final String PROPERTY_HTML_DEFINITION = "htmlDefinition";
	public static final String PROPERTY_ALTERNATIVE_DEFINITION = "alternativeDefinitions";

	private final int index;
	private final List<String> cachedURLs;
	private final List<String>  urls;
	private final static Pattern pattern = Pattern.compile("<font color=blue>(.+)</font>");
	private final List<PropertyChangeListener> changeListeners = new ArrayList<PropertyChangeListener>();
	private String definition;
	private List<CandidateDefinition> alternativeDefinitions;
	private String definitionHTMLFormatted;
	private int parentTermCount;
	private boolean isTicked;
	private boolean isVisible;


	/**
	 * Constructs a {@link CandidateDefinition}
	 * 
	 * @param index
	 * @param def
	 * @param ur
	 * @param cachedUR
	 */
	public CandidateDefinition(int index, String def, String defFormatted, String ur, String cachedUR)
	{
		this(index, def, defFormatted, ur, cachedUR, 0, false);
	}

	/**
	 * Constructs a {@link CandidateDefinition}
	 * 
	 * @param index
	 * @param def
	 * @param url
	 * @param cachedURL
	 * @param termCount
	 * @param select
	 */
	public CandidateDefinition(int index, String def, String defFormatted, String url, String cachedURL, int termCount, boolean select)
	{
		this(index,def,defFormatted, Collections.singletonList(url), Collections.singletonList(cachedURL), termCount, select);
	}
	
	
	
	/**
	 * Constructs a {@link CandidateDefinition}
	 * 
	 * @param index
	 * @param def
	 * @param urls
	 * @param cachedURLs
	 * @param termCount
	 * @param select
	 */
	public CandidateDefinition(int index, String def, String defFormatted, List<String> urls, List<String> cachedURLs, int termCount, boolean select)
	{
		this.urls = new ArrayList<String>();
		this.urls.addAll(urls);
		this.cachedURLs = new ArrayList<String>();
		this.cachedURLs.addAll(cachedURLs);
		this.index = index;
		this.definition = def;
		this.definitionHTMLFormatted = defFormatted;
		this.parentTermCount = termCount;
		this.isTicked = select;
		this.isVisible = true;
	}

	@Override
	public boolean equals(Object object)
	{
		return object instanceof CandidateDefinition && this.definition.equals(((CandidateDefinition) object).definition);
	}

	@Override
	public int hashCode()
	{
		return this.definition.hashCode();
	}

	/**
	 * @return the definitional context string parents should be searched in
	 */
	public String getDefinitionalContext()
	{
		// String htmlString =
		// getDefinitionHTMLFormatted().substring(this.startOfDefinitionalContext);
		// String replaced = htmlString.replaceAll("\\<.*?>","");
		Matcher matcher = pattern.matcher(getDefinitionHTMLFormatted());
		if (matcher.find()) {
			String match = matcher.group(1);
			Matcher matcher2 = pattern.matcher(match);
			if (matcher2.find()) {
				// sometimes server sense nested font tags
				return matcher2.group(1);
			}
			else {
				return matcher.group(1);
			}
		}
		return null;
	}

	/**
	 * >
	 * 
	 * @return the URL of the location where the definition was extracted from
	 */
	public List<String> getUrls()
	{
		return urls;
	}

	/**
	 * @return the cached URL of the location where the definition was extracted
	 *         from
	 */
	public List<String> getCachedURLs()
	{
		return cachedURLs;
	}

	/**
	 * @return the definition
	 */
	public String getDefinition()
	{
		return definition;
	}

	/**
	 * @return the index
	 */
	public int getIndex()
	{
		return index;
	}

	/**
	 * @return the number of known parents set
	 */
	public int getParentTermCount()
	{
		return parentTermCount;
	}

	/**
	 * @return <code>true</code> if the definition is ticked
	 */
	public boolean isTicked()
	{
		return isTicked;
	}

	/**
	 * @return <code>true</code> if the definition is currently set to visible
	 *         (e.g. by filtering)
	 */
	public boolean isVisible()
	{
		return isVisible;
	}

	/**
	 * Set the definition string
	 * 
	 * @param definition
	 *            the definition string
	 */
	public void setDefinition(String definition)
	{
		String oldValue = this.definition;
		this.definition = definition;
		notifyPropertyChangeListener(new PropertyChangeEvent(this, PROPERTY_DEFINITION, oldValue, this.definition));

	}


	public void addAlternativeDefinition(CandidateDefinition candidateDefinition)
	{
		if (alternativeDefinitions == null) {
			alternativeDefinitions = new ArrayList<CandidateDefinition>();
		}
		List<CandidateDefinition> oldValue = this.alternativeDefinitions;
		this.alternativeDefinitions.add(candidateDefinition);
		notifyPropertyChangeListener(new PropertyChangeEvent(this, PROPERTY_ALTERNATIVE_DEFINITION, oldValue, this.alternativeDefinitions));
	}

	public void resetAlternativeDefinitions()
	{
		alternativeDefinitions = null;
		notifyPropertyChangeListener(new PropertyChangeEvent(this, PROPERTY_ALTERNATIVE_DEFINITION, this.alternativeDefinitions, Collections.emptyList()));
	}

	public List<CandidateDefinition> getAlternativeDefinitions()
	{
		return alternativeDefinitions;
	}

	/**
	 * Set whether a definition is selected
	 * 
	 * @param isTicked
	 */
	public void setTicked()
	{
		boolean oldValue = this.isTicked;
		this.isTicked = true;
		notifyPropertyChangeListener(new PropertyChangeEvent(this, PROPERTY_SELECTION, oldValue, this.isTicked));
	}

	/**
	 * Set whether a definition is visible
	 * 
	 * @param isVisible
	 */
	public void setVisible(boolean isVisible)
	{
		this.isVisible = isVisible;
	}

	/**
	 * Add listeners to this {@link CandidateDefinition}
	 * 
	 * @param changeListener
	 */
	public void addPropertyChangeListener(PropertyChangeListener propertyChangeListener)
	{
		changeListeners.add(propertyChangeListener);
		if (changeListeners.size() > 1)
			throw new RuntimeException();
	}

	public void removePropertyChangeListeners()
	{
		changeListeners.clear();
	}

	/**
	 * Notify all attached listeners
	 */
	private void notifyPropertyChangeListener(PropertyChangeEvent event)
	{
		for (PropertyChangeListener l : changeListeners)
			l.propertyChange(event);
	}

	/**
	 * @param definitionHTMLFormatted
	 *            the definitionHTMLFormatted to set
	 */
	public void setDefinitionHTMLFormatted(String definitionHTMLFormatted)
	{
		String oldValue = this.definitionHTMLFormatted;
		this.definitionHTMLFormatted = definitionHTMLFormatted;
		notifyPropertyChangeListener(new PropertyChangeEvent(this, PROPERTY_HTML_DEFINITION, oldValue, this.definitionHTMLFormatted));
	}

	/**
	 * @return the definitionHTMLFormatted
	 */
	public String getDefinitionHTMLFormatted()
	{
		return definitionHTMLFormatted;
	}

	@Override
	public CandidateDefinition clone()
	{
		try {
			return (CandidateDefinition) super.clone();
		}
		catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return null;
	}

	public void addURLs(Collection<String> urls)
	{
		this.urls.addAll(urls);
	}

	public void addCachedURLs(Collection<String> cachedURLs)
	{
		this.cachedURLs.addAll(cachedURLs);
	}

	public void addURL(String url)
	{
		this.urls.add(url);	
	}

	public void addCachedURL(String cachedURL)
	{
		this.cachedURLs.add(cachedURL);
	}
}
