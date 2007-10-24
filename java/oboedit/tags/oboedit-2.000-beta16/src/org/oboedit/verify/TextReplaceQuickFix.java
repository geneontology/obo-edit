package org.oboedit.verify;

import org.obo.datamodel.FieldPath;

public interface TextReplaceQuickFix extends HistoryQuickFix {
	public String getNewText();
	public FieldPath getPath();
}
