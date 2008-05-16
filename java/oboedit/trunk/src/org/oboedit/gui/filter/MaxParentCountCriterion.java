package org.oboedit.gui.filter;

import java.util.Collection;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.filters.AbstractNumberCriterion;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.util.GUIUtil;

import org.apache.log4j.*;

public class MaxParentCountCriterion extends
	AbstractNumberCriterion<OBOSession> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MaxParentCountCriterion.class);

	protected int max = -1;

	public MaxParentCountCriterion() {
		GUIUtil.addReloadListener(new ReloadListener() {

			public void reload(ReloadEvent e) {
				if (e.isHistory() || e.isOntologyReload() || e.isRoot()) {
					recalculateMax();
				}
			}

		});
	}
	
	protected void recalculateMax() {
		max = 0;
		for (IdentifiedObject io : SessionManager.getManager()
				.getSession().getObjects()) {
			if (io instanceof LinkedObject) {
				int parentCount = ((LinkedObject) io).getParents()
						.size();
				if (parentCount > max)
					max = parentCount;
			}
		}
	}

	public String getID() {
		return "max_parent_count";
	}
	
	@Override
	public String toString() {
		return "Max parent count";
	}

	public Class<OBOSession> getInputType() {
		return OBOSession.class;
	}

	public Collection<Integer> getValues(Collection<Integer> scratch,
			OBOSession obj) {
		if (max < 0)
			recalculateMax();
		scratch.add(max);
		return scratch;
	}

}
