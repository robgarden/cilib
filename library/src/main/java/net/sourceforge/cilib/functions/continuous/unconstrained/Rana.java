/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained;

import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.functions.Differentiable;
import net.sourceforge.cilib.type.types.Numeric;
import net.sourceforge.cilib.type.types.Real;
import net.sourceforge.cilib.type.types.container.Vector;
import fj.F;

public class Rana extends ContinuousFunction {

    /**
     * {@inheritDoc}
     */
    @Override
    public Double f(Vector input) {
        double sum = 0;

        for (int j = 0; j < input.size() - 1; j++) {
            double xj = input.doubleValueOf(j);
            double xj1 = input.doubleValueOf(j + 1);
            double xjmod = input.doubleValueOf((j + 1) % input.size());

            double alpha = Math.sqrt(Math.abs(xj1 + 1 - xj));
            double beta  = Math.sqrt(Math.abs(xj1 + 1 + xj));

            sum += xj * Math.sin(alpha) * Math.cos(beta) + (xjmod + 1) * Math.cos(alpha) * Math.sin(beta);
        }

        return sum;        
    }
}
