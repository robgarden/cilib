/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.entity.initialisation.walks;

import net.sourceforge.cilib.entity.initialisation.InitialisationStrategy;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.type.types.Randomisable;
import net.sourceforge.cilib.type.types.Type;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.type.types.Bounds;
import net.sourceforge.cilib.math.random.UniformDistribution;

/**
 * Initialise entities by performing a random walk in the search space.
 */
public class RandomWalk<E extends Entity> implements InitialisationStrategy<E> {
    protected ControlParameter stepSize;
    protected Type previous;
    protected UniformDistribution uniform;

    public RandomWalk() {
        this.stepSize = ConstantControlParameter.of(1);
        this.uniform = new UniformDistribution();
    }

    @Override
    public RandomWalk getClone() {
        return this;
    }

    @Override
    public void initialise(Enum<?> key, E entity) {
        Type type = entity.getProperties().get(key);

        if (previous == null) {
            if (type instanceof Randomisable) {
                ((Randomisable)type).randomise();
                previous = type;
            } else {
                throw new UnsupportedOperationException(
                    "Cannot initialise a non Randomisable instance.");
            }
        } else {
            double step = stepSize.getParameter();
            Vector position = (Vector) type;
            for (int i = 0; i < position.size(); i++) {
                Bounds bounds = position.boundsOf(i);
                double value;

                do {
                    value = ((Vector)previous).doubleValueOf(i) +
                        uniform.getRandomNumber(-step, step);
                } while (!bounds.isInsideBounds(value));

                position.setReal(i, value);
            }
            previous = position;
        }
    }

    public void setStepSize(ControlParameter stepSize) {
        this.stepSize = stepSize;
    }
}
