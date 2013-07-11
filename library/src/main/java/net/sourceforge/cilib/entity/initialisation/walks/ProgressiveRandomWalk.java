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
 * Initialise entities more evenly than a random walk by enforcing a directional
 * walk bias.
 */
public class ProgressiveRandomWalk<E extends Entity> implements InitialisationStrategy<E> {
    protected ControlParameter stepSize;
    protected Vector previous, startingZone;
    protected UniformDistribution uniform;

    public ProgressiveRandomWalk() {
        this.stepSize = ConstantControlParameter.of(1);
        this.uniform = new UniformDistribution();
    }

    @Override
    public ProgressiveRandomWalk getClone() {
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
            for (int i = 0; i < current.size(); i++) {
                Bounds bounds = current.boundsOf(i);

                double r = uniform.getRandomNumber(0, stepSize.getParameter());
                if (startingZone.booleanValueOf(i)) {
                    r *= -1;
                }

                double value = previous.doubleValueOf(i) + r;

                if (!bounds.isInsideBounds(value)) {
                    double xMax = bounds.getUpperBound();
                    double xMin = bounds.getLowerBound();

                    // set to mirrored position inside boundary
                    if (value > xMax) {
                        value = xMax + (xMax - value);
                    } else {
                        value = xMin + (xMin - value);
                    }

                    // flip starting zone bit
                    startingZone.setBit(i, !startingZone.booleanValueOf(i));
                }

                current.setReal(i, value);
            }
        }

        // set previous walk to current walk
        previous = current;
    }

    public void setStepSize(ControlParameter stepSize) {
        this.stepSize = stepSize;
    }
}
