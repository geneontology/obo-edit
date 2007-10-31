package org.oboedit.graph;

import java.awt.Shape;
import java.awt.geom.GeneralPath;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.nodes.PPath;

public class PCNode extends PPath {

	public static final Object PATH_NODE = new Object();
	protected NamedChildProvider provider;

	protected PathCapable lo;
	protected PPath pathNode;

	public void initialize(PathCapable pc, NamedChildProvider provider, Shape s) {
		setOffset(0,0);
		setProvider(provider);
		setObject(pc);
		setPickable(true);
		pathNode = new PPath();
		pathNode.setPickable(false);
		setNamedChild(PATH_NODE, pathNode);
		setShape(s);
		setPaint(null);
		setStroke(null);
		setStrokePaint(null);
		setBounds(getPathDelegate().getBounds());
	}

	public void setShape(Shape s) {
		GeneralPath newShape = new GeneralPath();
		ShapeUtil.normalize(s, newShape);
		pathNode.setPathTo(newShape);
		setPathTo(newShape);
		setOffset(getXOffset() + s.getBounds2D().getX(), getYOffset()
				+ s.getBounds2D().getY());
		setBounds(getPathDelegate().getBounds());
	}
	
	@Override
	public double getWidth() {
		return getPathDelegate().getWidth();
	}

	@Override
	public double getHeight() {
		return getPathDelegate().getHeight();
	}
	
	public void setTooltipFactory(TooltipFactory factory) {
		addAttribute(TooltipFactory.KEY, factory);
	}

	public PPath getPathDelegate() {
		return (PPath) getNamedChild(PATH_NODE);
	}

	public void setObject(PathCapable lo) {
		this.lo = lo;
	}

	public PathCapable getObject() {
		return lo;
	}

	public void setProvider(NamedChildProvider provider) {
		this.provider = provider;
	}

	public PNode getNamedChild(Object key) {
		return provider.getNamedChild(key, this);
	}

	public void setNamedChild(Object name, PNode value) {
		provider.setNamedChild(name, this, value);
	}

	public NamedChildProvider getProvider() {
		return provider;
	}

}
