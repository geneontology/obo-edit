package org.bbop.dataadapter;

/**
 * A data adapter user interface. Implementations are not necessarily
 * visible components; a data adapter user interface may simply be a means
 * of interpreting command-line arguments or reading a configuration file.
 */

public interface DataAdapterUI {

    /*
     * Specifies a default adapter configuration to be loaded by this UI
     */
    public void setConfiguration(AdapterConfiguration config);

    /*
     * Called before getConfig() to give a UI a chance to set up.
     */
    public void init(AdapterWidgetI widget, IOOperation op,
		     DataAdapter adapter, Object input);

    /*
     * Reads the configuration set up by this ui. This configuration will
     * typically be passed straight to the doOperation() method of
     * a data adapter.
     */
    public AdapterConfiguration getConfig(IOOperation op, DataAdapter adapter,
					  Object input) throws
	DataAdapterUIException;

    /**
     * Creates an empty, uninitialized configuration.
     */
    public AdapterConfiguration createEmptyConfig();

    public UIConfiguration getUIConfiguration();

    public void setUIConfiguration(UIConfiguration uiconfig);

    /*
     * Called after getConfig() to give the UI a chance to clean up
     */
    public void cleanup();
}
