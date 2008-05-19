package org.bbop.dataadapter;

import java.util.*;

import org.bbop.util.*;

import org.apache.log4j.*;

public class CommandLineWidget implements AdapterWidgetI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CommandLineWidget.class);

	protected static CommandLineWidget self = new CommandLineWidget();

	private CommandLineWidget() {
	}

	public static TagSpec getTagSpec(final DataAdapterRegistry registry,
			DataAdapter defaultAdapter, IOOperation op, Object input) {
		TagSpec outerSpec = new TagSpec("-adapter", "<adapter options>");

		Class[] classes = { ParameterUI.class };

		DataAdapter[] adapters = DataAdapterUtil.getAdapters(registry,
				op, classes);
		DataAdapterUIFactory factory = registry.getUIFactory();

		for (int i = 0; i < adapters.length; i++) {
			TagSpec adapterSpec = new TagSpec("-" + adapters[i].getID());
			outerSpec.addArgumentSpec(adapterSpec, 1);
			if (adapters[i].equals(defaultAdapter))
				outerSpec.setImpliedSpec(adapterSpec, 1);

			Collection uis = factory.getUIs(adapters[i]);

			DataAdapterUI ui = DataAdapterUtil.getUI(adapters[i], registry,
					classes);
			if (ui == null)
				continue;

			ui.init(self, op, adapters[i], input);
			TagSpec spec = ((ParameterUI) ui).getParameterSpec().copy(
					"-options");
			ui.cleanup();
			adapterSpec.addArgumentSpec(spec, 1);
			adapterSpec.setImpliedSpec(spec, 1);
		}
		return outerSpec;
	}

	public static Object execute(DataAdapterRegistry registry, IOOperation op,
			Tag params, Object input)
			throws DataAdapterUIException, DataAdapterException {
		if (!params.getName().equals("-adapter")) {
			throw new DataAdapterUIException("Invalid argument: "
					+ params.getName() + ". "
					+ "CommandLineWidget requires an " + "-adapter argument");
		}
		Tag adapterTag = null;
		Tag adapterOptions = null;
		DataAdapter adapter = null;
		Class[] classes = { ParameterUI.class };

		Iterator it = params.getArguments().iterator();
		while (it.hasNext()) {
			Tag tag = (Tag) it.next();
			adapter = registry.getAdapter(tag.getName().substring(1));
			if (adapter != null) {
				adapterTag = tag;
				break;
			}
		}
		if (adapter == null)
			throw new DataAdapterUIException("No adapter specified!");
		it = adapterTag.getArguments().iterator();
		while (it.hasNext()) {
			Tag tag = (Tag) it.next();
			if (tag.getName().equals("-options")) {
				adapterOptions = tag;
				break;
			}
		}
		if (adapterOptions == null) {
			adapterOptions = new Tag("-options", true);
		}

		DataAdapterUI ui = null;

		ui = DataAdapterUtil.getUI(adapter, registry, classes);

		if (ui == null) {
			throw new DataAdapterException("Adapter " + adapter.getID()
					+ " does " + "not support command line " + "arguments.");
		}

		logger.info("adapter = " + adapter + ", adapterOptions = "
				+ adapterOptions);
		ui.init(self, op, adapter, input);
		((ParameterUI) ui).setParameters(adapterOptions);
		AdapterConfiguration config = ui.getConfig(op, adapter, input);
		ui.cleanup();

		Object output = adapter.doOperation(op, config, input);

		return output;
	}

}
