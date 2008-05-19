package org.obo.history;

import java.util.Collection;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class ObsoleteObjectHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ObsoleteObjectHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1088646280983365147L;

	public ObsoleteObjectHistoryItem() {
	}

	public ObsoleteObjectHistoryItem(IdentifiedObject o) {
		this(o.getID());
	}

	public ObsoleteObjectHistoryItem(String target) {
		this.target = target;
	}
	
	public boolean equals(Object o) {
		if (!(o instanceof ObsoleteObjectHistoryItem))
			return false;
		return ObjectUtil.equals(((ObsoleteObjectHistoryItem) o).getTarget(),
				target);
	}

	@Override
	public String getShortName() {
		return "obsolete";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
