package org.bbop.dataadapter;

import org.bbop.util.*;

public interface ParameterUI extends DataAdapterUI {

    public TagSpec getParameterSpec();

    /**
     * Returns the index where parameters were no longer accepted
     */
    public void setParameters(Tag optionsTag) throws DataAdapterUIException;
}
