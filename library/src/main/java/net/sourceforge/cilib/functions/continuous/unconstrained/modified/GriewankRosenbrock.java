/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;

/**
 * Composite Griewank-Rosenbrock function as specified in reference.
 *
 * <p>
 * Reference:
 * </p>
 * <p>
 * Hansen, Nikolaus, et al. "Real-parameter black-box optimization
 * benchmarking 2009: Noiseless functions definitions." (2009).
 * </p>
 *
 */
public class GriewankRosenbrock implements ContinuousFunction {

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        double sum = 0;

        for (int i = 0; i < input.size() - 1; i++) {
            double z = input.doubleValueOf(i);
            double z1 = input.doubleValueOf(i + 1);
            double s = 100 * Math.pow(z * z - z1, 2) + (z - 1) * (z - 1);
            sum += (s / 4000) - Math.cos(s);
        }

        return (10.0 / input.size() - 1) * sum + 10;
    }
}
