package org.oboedit.piccolo;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

public class MorphableMorpher extends DefaultMorpher {
	@Override
	public PCompoundActivity morph(PNode oldNode, PNode newNode, long duration) {

		if (oldNode instanceof Morphable) {
			PCompoundActivity out;
			if (((Morphable) oldNode).doDefaultMorph())
				out = super.morph(oldNode, newNode, duration);
			else
				out = new PCompoundActivity();
			PActivity activity = ((Morphable) oldNode).morphTo(newNode,
					duration);
			out.addActivity(activity);
			return out;
		} else
			return super.morph(oldNode, newNode, duration);
	}
}
