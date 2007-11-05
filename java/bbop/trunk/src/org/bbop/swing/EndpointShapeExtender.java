package org.bbop.swing;

import java.awt.geom.PathIterator;

public class EndpointShapeExtender extends AbstractShapeExtender {

	protected void addPoints(PathOpList source, int startSourceIndex,
			int endSourceIndex, PathOpList target, int startTargetIndex,
			int endTargetIndex) {
		PathOpList smallerList;
		int newNodeCount;
		int smallerStartIndex;
		int smallerEndIndex;

		if (endSourceIndex - startSourceIndex == endTargetIndex
				- startTargetIndex) {
			return;
		}
		if (endSourceIndex - startSourceIndex < endTargetIndex
				- startTargetIndex) {
			newNodeCount = (endTargetIndex - startTargetIndex)
					- (endSourceIndex - startSourceIndex);
			smallerStartIndex = startSourceIndex;
			smallerEndIndex = endSourceIndex;
			smallerList = source;
		} else {
			newNodeCount = (endSourceIndex - startSourceIndex)
					- (endTargetIndex - startTargetIndex);
			smallerStartIndex = startTargetIndex;
			smallerEndIndex = endTargetIndex;
			smallerList = target;
		}
		int endNodes = newNodeCount / 2;
		int frontNodes = newNodeCount - endNodes;
		for (int i = 0; i < frontNodes; i++)
			smallerList.duplicateEndPoint(smallerStartIndex);
		for (int i = 0; i < endNodes; i++)
			smallerList.duplicateEndPoint(smallerEndIndex);

	}

}
