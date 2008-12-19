package org.oboedit.gui.components.ontologyGeneration;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Candidate Term Cache
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), Dec 16, 2008
 */
public class CandidateTermCache
{
	private Map<String, CandidateTerm> cache = new HashMap<String, CandidateTerm>();

	/**
	 * Add a term to the cache
	 * 
	 * @param candidateTerm the {@link CandidateTerm} to add
	 */
	public void addTerm(CandidateTerm candidateTerm)
	{
		cache.put(candidateTerm.getGeneratedLabel().toLowerCase(), candidateTerm);
	}

	/**
	 * Get a {@link CandidateTerm} from cache
	 * 
	 * @param key
	 * @return the {@link CandidateTerm} for the given key
	 */
	public CandidateTerm get(String key)
	{
		return cache.get(key.toLowerCase());
	}

	/**
	 * Determines whether a {@link CandidateTerm} exists in the cache
	 * 
	 * @param candidateTerm the {@link CandidateTerm} to check
	 * @return <code>true</code> if {@link CandidateTerm} is contained
	 */
	public boolean hasCandidateTerm(CandidateTerm candidateTerm)
	{
		return cache.containsKey(candidateTerm.getGeneratedLabel().toLowerCase());
	}

	/**
	 * Determines whether a {@link CandidateTerm} with a specific label exists in the cache
	 * 
	 * @param label the label to check
	 * @return <code>true</code> if a {@link CandidateTerm} with this label is contained
	 */
	public boolean hasCandidateTermWithLabel(String label)
	{
		return cache.containsKey(label.toLowerCase());
	}

	/**
	 * Returns the chache size
	 * 
	 * @return number of elements in the cache
	 */
	public int getSize()
	{
		return cache.size();
	}

	/**
	 * Clear the cache
	 */
	public void clear()
	{
		cache.clear();
	}

	/**
	 * Remove a term from the cache
	 * 
	 * @param term the {@link CandidateTerm} to be removed
	 */
	public void removeTerm(CandidateTerm term)
	{
		cache.remove(term.getGeneratedLabel().toLowerCase());
	}

	/**
	 * Get all cached {@link CandidateTerm}s
	 * 
	 * @return a {@link Collection} of {@link CandidateTerm}
	 */
	public Collection<CandidateTerm> getAllCandidateTerms()
	{
		return Collections.unmodifiableCollection(cache.values());
	}
}
