package org.obo.reasoner;

import java.awt.event.*;
import java.util.*;

import org.bbop.util.ProgressValued;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;

/**
 * A {@link LinkDatabase} that is backed by a reasoner. A reasoner is an
 * algorithm that can derive implied information by examining the explicitly
 * stated information in an ontology. In this api, that implied information
 * takes the form of new links or terms that are added to the
 * ReasonedLinkDatabase.
 * 
 * A ReasonedLinkDatabase will not edit the existing ontology. All the derived
 * information discovered by the reasoner is stored within the
 * ReasonedLinkDatabase itself.
 * 
 * @author jrichter
 * 
 */
public interface ReasonedLinkDatabase extends LinkDatabase, ProgressValued {


	/**
	 * Sets the {@link LinkDatabase} that the reasoner will use to discover
	 * implied terms and links.
	 * 
	 * @param linkDatabase
	 */
	public void setLinkDatabase(LinkDatabase linkDatabase);

	/**
	 * Returns the {@link LinkDatabase} that the reasoner is using to discover
	 * implied terms and links.
	 * 
	 * @param linkDatabase
	 */
	public LinkDatabase getLinkDatabase();

	/**
	 * (Optionally) returns a collection of explanations for why a particular
	 * object exists. If the object is an explicitly stated ontology object, the
	 * collection should contain at least {@link Explanation#GIVEN_EXPLANATION}
	 * to indicate that this link was not created by the reasoner.
	 * 
	 * A link may have many explanations, because a single link may be derivable
	 * by several different reasoner rules, and an ontology may have explicitly
	 * stated a link that could have been derived by the reasoner.
	 * 
	 * @param link
	 * @return
	 */
	public Collection<Explanation> getExplanations(PathCapable link);

	/**
	 * Whether {@link OBOClass} a is a subclass of {@link OBOClass} b.
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean isSubclassOf(OBOClass a, OBOClass b);

	/**
	 * Whether {@link OBOProperty} a is a subproperty of {@link OBOProperty} b.
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean isSubPropertyOf(OBOProperty a, OBOProperty b);

	/**
	 * Whether {@link Instance} a is an instance of {@link OBOClass} b.
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean isInstanceOf(Instance a, OBOClass b);


	/**
	 * Returns all the parents (including parents implied by transitivity) of
	 * {@link LinkedObject} a of {@link OBOProperty type} prop.
	 * 
	 * @param a
	 * @param prop
	 * @return
	 */
	public Collection<LinkedObject> getParentsOfType(LinkedObject a,
			OBOProperty prop);

	/**
	 * Whether a link is redundant. A link is considered redundant if it is
	 * explicitly stated in the ontology, but could have been automatically
	 * derived by the reasoner.
	 * 
	 * @param link
	 * @return
	 */
	public boolean isRedundant(Link link);

	/**
	 * Adds a link to this {@link ReasonedLinkDatabase}. Most reasoners will
	 * just call {@link #recache()} to implement this method, but reasoners that
	 * can dynamically integrate new information will need to override this
	 * method.
	 * 
	 * @param link
	 */
	public void addLink(Link link);

	/**
	 * Removes a link from this {@link ReasonedLinkDatabase}. Most reasoners
	 * will just call {@link #recache()} to implement this method, but reasoners
	 * that can dynamically integrate new information will need to override this
	 * method.
	 * 
	 * @param link
	 */
	public void removeLink(Link link);

	/**
	 * Reruns the reasoning process implemented by this
	 * {@link ReasonedLinkDatabase}. All cached information is discarded and
	 * the entire reasoning process is redone.
	 * 
	 * When recache is called, an {@link ActionEvent} is fired on all
	 * {@link ActionListener}s registered with this reasoner.
	 * 
	 * @return
	 */
	public long recache();
	
	public boolean isRunning();
	
	public boolean isCancelled();
	
	public void cancel();
	
	public void addReasonerListener(ReasonerListener listener);

	public void removeReasonerListener(ReasonerListener listener);
}
