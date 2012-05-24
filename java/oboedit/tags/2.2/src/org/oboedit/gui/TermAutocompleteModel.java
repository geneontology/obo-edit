package org.oboedit.gui;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.bbop.util.FastSuperset;
import org.bbop.util.Superset;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.filters.NameSearchCriterion;
import org.obo.filters.SynonymSearchCriterion;
import org.oboedit.controller.SessionManager;

import org.apache.log4j.*;

public class TermAutocompleteModel extends
	AbstractQueryAutocompleteModel<FieldPath, IdentifiedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermAutocompleteModel.class);

	protected Map<FieldPathSpec, Double> pathSpecs = new LinkedHashMap<FieldPathSpec, Double>();

	public TermAutocompleteModel() {
            // What are the 5 and the 1 for?
		addPathSpec(new FieldPathSpec(NameSearchCriterion.CRITERION), 5);
		addPathSpec(new FieldPathSpec(SynonymSearchCriterion.CRITERION), 1);
	}
	
	public void clearPathSpecs() {
		pathSpecs.clear();
	}
	
	public void addPathSpec(FieldPathSpec spec) {
		addPathSpec(spec, 1);
	}
	
	@Override
	public double getWeight(FieldPath val) {
		return pathSpecs.get(val.getSpec());
	}
	
	public void addPathSpec(FieldPathSpec spec, double weight) {
		pathSpecs.put(spec, weight);
	}
	
	public LinkDatabase getLinkDatabase() {
		return SessionManager.getManager().getSession().getLinkDatabase();
	}
	
	public FieldPath createValue(String val) {
		throw new UnsupportedOperationException();
	}
	
	public Class<FieldPath> getDisplayType() {
		return FieldPath.class;
	}

	public List<FieldPath> getDisplayValues(IdentifiedObject val) {
		List<FieldPath> out = new LinkedList<FieldPath>();
		for(FieldPathSpec spec : pathSpecs.keySet()) {
			FieldPath queryPath = spec.createQueryPath(val);
			out.addAll(FieldPath.resolve(queryPath, getLinkDatabase()));
		}
		return out;
	}

	public Class<IdentifiedObject> getOutputType() {
		return IdentifiedObject.class;
	}

	public IdentifiedObject getOutputValue(FieldPath val) {
		return val.getObject();
	}

	public boolean isLegal(FieldPath val) {
		return getAllValues().contains(val);
	}

	public String toString(FieldPath val) {
	    if (val.getLastValue() == null)
		return "";
            Object lastVal = val.getLastValue();
            //            logger.debug("TermAutocompleteModel: fpObj = " + val.getObject() + "; lastVal " + lastVal + " is a " + lastVal.getClass().getName()); // DEL

            Object fpObj = val.getObject();

            if (fpObj instanceof OBOClass) {
                OBOClass ob = (OBOClass)fpObj;
                //                logger.debug("TermAutocompleteModel: ob " + ob + "; name = " + ob.getName() + "; id = " + ob.getID()); // DEL
		if (ob.getName() == null)
                    return ob.getID();
                else if (lastVal instanceof SynonymImpl) {
                    SynonymImpl syn = (SynonymImpl) lastVal;
                    // Include synonym name AND ID, and also an indication that this is a synonym.
                    return syn.toString() + " (" + ob.getID() + ")--synonym for " + ob.getName();
                }
                else {
                    // I kind of feel like there should be a way to filter these unwanted
                    // things out before we get to this point, but I didn't find an obvious way.
                    if (ob.getName().startsWith("obo:"))
                        return "";
                    // Include term name AND ID
                    return ob.getName() + " (" + ob.getID() + ")";
                }
            }
            else // If it's not an OBOClass, we're not interested in it (it's a relation or something built-in)
                return "";
        }

	@Override
	public Collection<FieldPath> getQueryInput() {
		Superset<FieldPath> paths = new FastSuperset<FieldPath>();
		for (FieldPathSpec spec : pathSpecs.keySet()) {
			Collection<FieldPath> fpaths = FieldPath.resolve(spec,
					getLinkDatabase());
			paths.addSubset(fpaths);
		}
		return paths;
	}
}
