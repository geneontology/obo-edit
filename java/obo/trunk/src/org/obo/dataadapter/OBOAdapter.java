package org.obo.dataadapter;

import java.util.List;

import org.bbop.dataadapter.*;
import org.obo.datamodel.OBOSession;
import org.obo.history.HistoryList;

public interface OBOAdapter extends DataAdapter {
	public static final IOOperation<HistoryList, Void> WRITE_HISTORY = new DefaultIOOperation<HistoryList, Void>(
			"WRITE_HISTORY", "Write history", HistoryList.class, Void.class);

	@SuppressWarnings("unchecked")
	public static final IOOperation<Void, List<HistoryList>> READ_HISTORY = (IOOperation<Void, List<HistoryList>>) (new DefaultIOOperation(
			"READ_HISTORY", "Read history", Void.class, List.class));

	public static final IOOperation<OBOSession, OBOSession> WRITE_ONTOLOGY = new DefaultIOOperation<OBOSession, OBOSession>(
			"WRITE_ONTOLOGY", "Write ontology", OBOSession.class,
			OBOSession.class);

	public static final IOOperation<Void, OBOSession> READ_ONTOLOGY = new DefaultIOOperation<Void, OBOSession>(
			"READ_ONTOLOGY", "Read ontology", Void.class, OBOSession.class);
}
