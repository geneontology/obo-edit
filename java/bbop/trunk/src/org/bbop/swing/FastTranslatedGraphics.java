package org.bbop.swing;

import java.awt.*;
import java.awt.image.*;
import java.text.*;

import org.apache.log4j.*;

public class FastTranslatedGraphics extends Graphics {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FastTranslatedGraphics.class);

    protected Graphics g;
    int tx;
    int ty;

    public FastTranslatedGraphics(Graphics g) {
	this.g = g;
	tx = 0;
	ty = 0;
    }

    public void translateAbsolute(int x, int y) {
	tx = x;
	ty = y;
    }

    @Override
	public void translate(int x, int y) {
	g.translate(x,y);
    }

    @Override
	public void clearRect(int x, int y, int width, int height) {
	g.clearRect(x+tx,y+ty,width,height);
    }

    @Override
	public void clipRect(int x, int y, int width, int height) {
	g.clipRect(x+tx,y+ty,width,height);
    }

    @Override
	public void copyArea(int x, int y, int width, int height,
            int dx, int dy) {
	g.copyArea(x+tx,y+ty,width,height,dx,dy);
    }

    @Override
	public Graphics create() {
	return g.create();
    }

    @Override
	public Graphics create(int x, int y, int width, int height) {
	return g.create(x+tx,y+ty,width,height);
    }

    @Override
	public void dispose() {
	g.dispose();
    }

    @Override
	public void draw3DRect(int x, int y, int width, int height,
            boolean raised) {
	g.draw3DRect(x+tx,y+ty,width,height,raised);
    }

    @Override
	public void drawArc(int x, int y, int width, int height,
            int startAngle, int arcAngle){
	g.drawArc(x+tx,y+ty,width,height,startAngle,arcAngle);
    }
    @Override
	public void drawBytes(byte[] data, int offset, int length,
            int x, int y) {
	g.drawBytes(data,offset,length,x+tx,y+ty);
    }
    @Override
	public void drawChars(char[] data, int offset, int length,
            int x, int y) {
	g.drawChars(data,offset,length,x+tx,y+ty);
    }
    @Override
	public boolean drawImage(Image img, int x, int y,
            Color bgcolor, ImageObserver observer){
	return g.drawImage(img, x+tx,y+ty,bgcolor,observer); 
    }

    @Override
	public boolean drawImage(Image img, int x, int y,
            ImageObserver observer){
	return g.drawImage(img, x+tx,y+ty,observer); 
    }

    @Override
	public boolean drawImage(Image img, int x, int y,int width,int height,
            Color bgcolor, ImageObserver observer){
	return g.drawImage(img, x+tx,y+ty,width,height,bgcolor,observer);
    }

    @Override
	public boolean drawImage(Image img, int x, int y,int width,int height,
            ImageObserver observer){
	return g.drawImage(img, x+tx,y+ty,width,height,observer);
    }

    @Override
	public boolean drawImage(Image img, int dx1, int dy1, int dx2,
			     int dy2, int sx1, int sy1, int sx2, int sy2,
			     Color bgcolor, ImageObserver observer) {
	return g.drawImage(img, dx1+tx, dy1+ty, dx2+tx, dy2+ty,
			   sx1, sy1, sx2, sy2, bgcolor, observer);
    }

    @Override
	public boolean drawImage(Image img, int dx1, int dy1, int dx2,
			     int dy2, int sx1, int sy1, int sx2, int sy2,
			     ImageObserver observer) {
	return g.drawImage(img, dx1+tx, dy1+ty, dx2+tx, dy2+ty,
			   sx1, sy1, sx2, sy2, observer);
    }

    @Override
	public void drawLine(int x1, int y1, int x2, int y2) {
	g.drawLine(x1+tx, y1+ty, x2+tx, y2+ty);
    }

    @Override
	public void drawOval(int x, int y, int width, int height){
	g.drawOval(x+tx, y+ty, width, height);
    }

    @Override
	public void drawPolygon(int[] xPoints, int[] yPoints,
            int nPoints) {
	// do nothing
    }

    @Override
	public void drawPolygon(Polygon p) {
	// do nothing
    }

    @Override
	public void drawPolyline(int[] xPoints, int[] yPoints,
            int nPoints) {
	// do nothing
    }

    @Override
	public void drawRect(int x, int y, int width, int height) {
	g.drawRect(x+tx, y+ty, width, height);
    }

    @Override
	public void drawRoundRect(int x, int y, int width,
            int height, int arcWidth, int arcHeight) {
	g.drawRoundRect(x+tx, y+ty, width, height, arcWidth, arcHeight);
    }

/* Changed so that it can be compiled with 1.1. To get round the absence 
   of java.text.AttributedCharacterIterator, a dummy version of this class
   has been created in apollo/ver1.1/apollo/gui and apollo/ver1.1 added to
   the CLASSPATH for compiling with java version 1.1.
   This is a hack.

   NOTE: The method is NON functional
*/
    @Override
	public void drawString(AttributedCharacterIterator iterator,
            int x, int y) {
       // does nothing
       // g.drawString(iterator, x+tx, y+ty);
    }

    @Override
	public void drawString(String str, int x, int y) {
	g.drawString(str, x+tx, y+ty);
    }

    @Override
	public void fill3DRect(int x, int y, int width, int height,
            boolean raised) {
	g.fill3DRect(x+tx, y+ty, width, height, raised);
    }

    @Override
	public void fillArc(int x, int y, int width, int height,
            int startAngle, int arcAngle) {
	g.fillArc(x+tx, y+ty, width, height, startAngle, arcAngle);
    }

    @Override
	public void fillOval(int x, int y, int width, int height) {
	g.fillOval(x+tx, y+ty, width, height);
    }

    @Override
	public void fillPolygon(int[] xPoints, int[] yPoints,
            int nPoints) {
	// do nothing
    }

    @Override
	public void fillPolygon(Polygon p) {
	// do nothing
    }

    @Override
	public void fillRect(int x, int y, int width, int height){
	g.fillRect(x+tx, y+ty, width, height);
    }

    @Override
	public void fillRoundRect(int x, int y, int width,
            int height, int arcWidth, int arcHeight) {
	g.fillRoundRect(x+tx, y+ty, width, height, arcWidth, arcHeight);
    }

    @Override
	public void finalize() {
	g.finalize();
	super.finalize();
    }

    @Override
	public Shape getClip() {
	return g.getClip();
    }

    @Override
	public Rectangle getClipBounds() {
	return g.getClipBounds();
    }

    @Override
	public Rectangle getClipBounds(Rectangle r) {
        Rectangle clipRect = g.getClipBounds();
        r.x = clipRect.x;
        r.y = clipRect.y;
        r.width = clipRect.width;
        r.height = clipRect.height;
        return r;
    }

    /**
     * @deprecated As of JDK 1.2
     */
    @Deprecated
	@Override
	public Rectangle getClipRect() {
	return g.getClipBounds();
    }

    @Override
	public Color getColor() {
	return g.getColor();
    }

    @Override
	public Font getFont() {
	return g.getFont();
    }

    @Override
	public FontMetrics getFontMetrics() {
	return g.getFontMetrics();
    }

    @Override
	public FontMetrics getFontMetrics(Font f) {
	return g.getFontMetrics(f);
    }

    @Override
	public boolean hitClip(int x, int y, int width, int height) {
        return new Rectangle(x,y,width,height).intersects(g.getClipBounds());
    }

    @Override
	public void setClip(int x, int y, int width, int height) {
	g.setClip(x+tx,y+ty,width,height);
    }

    @Override
	public void setClip(Shape clip) {
	// do nothing
    }

    @Override
	public void setColor(Color c) {
	g.setColor(c);
    }

    @Override
	public void setFont(Font f) {
	g.setFont(f);
    }

    @Override
	public void setPaintMode() {
	g.setPaintMode();
    }

    @Override
	public void setXORMode(Color c1) {
	g.setXORMode(c1);
    }

    @Override
	public String toString() {
	return g.toString()+" [wrapped tranlation "+tx+", "+ty+"]";
    }
}
