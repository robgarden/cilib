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
import net.sourceforge.cilib.functions.continuous.unconstrained.Rastrigin;

/**
 * Buche-Rastrigin function as specified in reference.
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
public class BucheRastrigin implements ContinuousFunction {
    private Rastrigin rastrigin;

    public BucheRastrigin() {
        this.rastrigin = new Rastrigin();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        Vector.Builder builder = Vector.newBuilder();
        for (int i = 0; i < input.size(); i++) {
            double x = input.doubleValueOf(i);
            double s = Math.pow(10, 0.5 * (i / (input.size()-1)));

            if ((x > 0) && (i % 2 == 0)) {
                builder.add(10 * s * x);
            } else {
                builder.add(s * x);
            }
        }
        
        return rastrigin.apply(builder.build());
    }
}
