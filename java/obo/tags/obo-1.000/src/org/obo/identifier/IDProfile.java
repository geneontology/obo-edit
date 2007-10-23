package org.obo.identifier;

import java.util.List;
import java.io.Serializable;

public interface IDProfile extends Serializable {

	public void addRule(IDRule rule);

	public void setRules(List rules);

	public List getRules();

	public String getDefaultRule();

	public void setDefaultRule(String defaultRule);

	public void setName(String name);

	public String getName();
}
