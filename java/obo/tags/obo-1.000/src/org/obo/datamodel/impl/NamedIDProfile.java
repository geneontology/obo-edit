package org.obo.datamodel.impl;

import java.util.LinkedList;
import java.util.List;

import org.obo.datamodel.*;
import org.obo.identifier.IDProfile;
import org.obo.identifier.IDRule;

public class NamedIDProfile implements IDProfile {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6151111376324635711L;
	protected String name;
	protected String defaultRule;
	protected List rules = new LinkedList();

	public NamedIDProfile() {
	}

	public NamedIDProfile(String name) {
		setName(name);
	}

	public void setRules(List rules) {
		this.rules = rules;
	}

	public void addRule(IDRule rule) {
		rules.add(rule);
	}

	public List getRules() {
		return rules;
	}

	public String getDefaultRule() {
		return defaultRule;
	}

	public void setDefaultRule(String defaultRule) {
		this.defaultRule = defaultRule;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o != null && o instanceof NamedIDProfile) {
			return ((NamedIDProfile) o).getName().equals(getName());
		} else
			return false;
	}

	@Override
	public String toString() {
		return getName();
	}
}
