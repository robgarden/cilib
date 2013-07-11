/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.entity.initialisation.walks;

import net.sourceforge.cilib.entity.initialisation.InitialisationStrategy;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.type.types.Type;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.type.types.Bounds;
import net.sourceforge.cilib.math.random.UniformDistribution;
import net.sourceforge.cilib.math.random.DiscreteUniformDistribution;
import net.sourceforge.cilib.util.Vectors;

/**
 * Initialise entities by performing a Manhattan walk in the search space with a
 * fixed step size.
 */
public class ManhattanProgressiveRandomWalk<E extends Entity> implements InitialisationStrategy<E> {
    protected ControlParameter stepSize;
    protected Vector previous, startingZone;
    protected UniformDistribution uniform;

    public ManhattanProgressiveRandomWalk() {
        this.stepSize = ConstantControlParameter.of(1);
        this.uniform = new UniformDistribution();
    }

    @Override
    public ManhattanProgressiveRandomWalk getClone() {
        return this;
    }

    @Override
    public void initialise(Enum<?> key, E entity) {
        Type type = entity.getProperties().get(key);
        Vector current = (Vector) type;

        // initialise starting zone
        if (startingZone == null) {
            startingZone = Vectors.distributedVector(current.size(),
                new DiscreteUniformDistribution());
        }

        if (previous == null) {
            // first step in walk
            for (int i = 0; i < current.size(); i++) {
                Bounds bounds = current.boundsOf(i);
                double r = uniform.getRandomNumber(0, bounds.getRange() / 2);

                if (startingZone.booleanValueOf(i)) {
                    current.setReal(i, bounds.getUpperBound() - r);
                } else {
                    current.setReal(i, bounds.getLowerBound() + r);
                }
            }

            int rd = Vector.newBuilder().range(0, current.size(), 1)
                .build().sample().intValue();

            if (startingZone.booleanValueOf(rd)) {
                current.setReal(rd, current.boundsOf(rd).getUpperBound());
            } else {
                current.setReal(rd, current.boundsOf(rd).getLowerBound());
            }
        } else {
            int rd = Vector.newBuilder().range(0, current.size(), 1)
                .build().sample().intValue();

            for (int i = 0; i < current.size(); i++) {
                double prevValue = previous.doubleValueOf(i);
                if (i == rd) {
                    int sign = startingZone.booleanValueOf(i) ? -1 : 1;
                    double value = prevValue + sign * stepSize.getParameter();

                    if (!current.boundsOf(i).isInsideBounds(value)) {
                        value = prevValue - sign * stepSize.getParameter();
                        startingZone.setBit(i, !startingZone.booleanValueOf(i));
                    }
                    current.setReal(i, value);
                } else {
                    current.setReal(i, prevValue);
                }
            }
        }

        // set previous walk to current walk
        previous = current;
    }

    public void setStepSize(ControlParameter stepSize) {
        this.stepSize = stepSize;
    }
}
