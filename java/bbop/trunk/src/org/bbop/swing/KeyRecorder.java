package org.bbop.swing;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class KeyRecorder implements KeyEventDispatcher {

    public static final int MAX_KEYS = 10;

    protected int [] codeArray = new int[MAX_KEYS];
    protected int arrayLength = 0;
    protected Vector listeners = new Vector();
    protected KeyChecker keyChecker = new KeyChecker();
    protected CoalesceThread coalesceThread;

    public class KeyChecker {
	public boolean isDown(int keyCode) {
	    return containsKey(keyCode);
	}
    }

    public KeyRecorder() {
    }

    protected void addKey(int key) {
	if (arrayLength < codeArray.length)
	    codeArray[arrayLength++] = key;
    }

    protected void removeKey(int key) {
	for(int i=0; i < arrayLength; i++)
	    if (codeArray[i] == key)
		codeArray[i] = codeArray[--arrayLength];
    }

    protected boolean containsKey(int key) {
	for(int i=0; i < arrayLength; i++)
	    if (codeArray[i] == key)
		return true;
	return false;
    }

    public void setSmoothOutAutorepeat(boolean val) {
	if (val != smoothOutAutorepeat) {
	    smoothOutAutorepeat = val;
	    keyTimings.clear();
	    if (smoothOutAutorepeat) {
		coalesceThread = new CoalesceThread();
		coalesceThread.start();
	    } else {
		coalesceThread.halt();
		coalesceThread = null;
	    }
	}
    }

    public void setCoalesceInterval(long coalesceInterval) {
	this.coalesceInterval = coalesceInterval;
    }

    protected boolean smoothOutAutorepeat = false;

    KeyEvent lastPress;
    protected long coalesceInterval = 500;
    protected Map keyTimings = Collections.synchronizedMap(new HashMap());
    protected HashSet sentKeys = new HashSet();

    protected class KeyEventHolder {
	KeyEvent event;
	long timereceived;
	boolean consumed;

	public KeyEventHolder(KeyEvent event, long timereceived,
			      boolean consumed) {
	    this.event = event;
	    this.timereceived = timereceived;
	    this.consumed = consumed;
	}			      
    }


    protected class CoalesceThread extends Thread {
	protected HashSet removem = new HashSet();
	protected HashSet examinem = new HashSet();
	protected boolean halt = false;

	public CoalesceThread() {
	    setDaemon(true);
	    setPriority(Thread.MIN_PRIORITY);
	}

	protected void halt() {
	    halt = true;
	}

	public void run() {
	    while(!halt) {
		try {
		    removem.clear();
		    examinem.clear();
		    
		    synchronized (KeyRecorder.this) {
			examinem.addAll(keyTimings.keySet());
			Iterator it = examinem.iterator();
			while(it.hasNext()) {
			    Integer key = (Integer) it.next();
			    KeyEventHolder e = (KeyEventHolder)
				keyTimings.get(key);
			    long timeDiff = System.currentTimeMillis() -
				e.timereceived;
			    if (timeDiff >
				coalesceInterval) {
				if (!e.consumed) {
				    processKeyEvent(e.event);
				}
				e.timereceived = timeDiff;
				removem.add(key);
			    }
			}
			it = removem.iterator();
			while(it.hasNext()) {
			    Object o = it.next();
			    KeyEventHolder keh = (KeyEventHolder)
				keyTimings.remove(o);
			    keyTimings.remove(o);
			}
		    }
		    sleep(coalesceInterval/2);
		} catch (Exception ex) {
		    ex.printStackTrace();
		}
	    }
	    arrayLength = 0;
	}
    };

    protected synchronized void processKeyEvent(KeyEvent e) {
//	System.err.println("processed "+e+", smooth = "+smoothOutAutorepeat);
	if (e.getID() == KeyEvent.KEY_PRESSED) {
	    addKey(e.getKeyCode());
	} else if (e.getID() == KeyEvent.KEY_RELEASED) {
	    removeKey(e.getKeyCode());
	}
	for(int i=0; i < listeners.size(); i++) {
	    KeyListener kl = (KeyListener) listeners.get(i);
	    if (e.getID() == KeyEvent.KEY_PRESSED) {
		kl.keyPressed(e);
	    } else if (e.getID() == KeyEvent.KEY_RELEASED) {
		kl.keyReleased(e);
	    } else if (e.getID() == KeyEvent.KEY_TYPED) {
		kl.keyTyped(e);
	    }
	}
    }

    public synchronized boolean dispatchKeyEvent(KeyEvent e) {
	if (smoothOutAutorepeat) {
	    if (e.getID() == KeyEvent.KEY_PRESSED ||
		e.getID() == KeyEvent.KEY_RELEASED) {
		long time = System.currentTimeMillis();
		Integer keyint = new Integer(e.getKeyCode());
		boolean consumed = false;
		if (!keyTimings.containsKey(keyint)) {
		    processKeyEvent(e);
		    consumed = true;
		}
		KeyEventHolder keh = new KeyEventHolder(e, 0, consumed);
		keyTimings.put(keyint, keh);
		keh.timereceived = System.currentTimeMillis();
	    }
	} else {
	    processKeyEvent(e);
	}
	return false;
    }

    public KeyChecker getKeyChecker() {
	return keyChecker;
    }

    public boolean isDown(int keyCode) {
	return keyChecker.isDown(keyCode);
    }

    public void addKeyListener(KeyListener kl) {
	listeners.add(kl);
    }

    public void removeKeyListener(KeyListener kl) {
	listeners.remove(kl);
    }

    public void install() {
	KeyboardFocusManager.getCurrentKeyboardFocusManager().
	    addKeyEventDispatcher(this);
    }

    public void uninstall() {
	KeyboardFocusManager.getCurrentKeyboardFocusManager().
	    removeKeyEventDispatcher(this);
    }
}
