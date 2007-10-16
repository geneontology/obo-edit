package org.obo.history;

import org.obo.datamodel.OBOSession;

public interface OperationModel {

	public void setSession(OBOSession history);

	public OperationWarning apply(HistoryItem item);

	public OperationWarning reverse(HistoryItem item);
	
	public void addLockstepModel(OperationModel model);
	
	public void removeLockstepModel(OperationModel model);
}
