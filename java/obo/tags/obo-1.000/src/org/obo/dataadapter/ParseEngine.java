package org.obo.dataadapter;

import java.io.IOException;
import java.util.Collection;

import org.bbop.util.ProgressValued;

public interface ParseEngine extends ProgressValued {

	public abstract void cancel();

	public abstract void setPaths(Collection<String> paths);

	public abstract void setParser(OBOSimpleParser parser);

	public abstract void parse() throws IOException, OBOParseException;

	public abstract void parse(String path) throws IOException,
			OBOParseException;

	public abstract int getLineNum();

	public abstract String getCurrentLine();

}