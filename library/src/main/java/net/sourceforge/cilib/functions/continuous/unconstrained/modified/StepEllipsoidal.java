/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import fj.F;
import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.functions.continuous.unconstrained.Elliptic;
import net.sourceforge.cilib.functions.continuous.decorators.RotatedFunctionDecorator;
import net.sourceforge.cilib.type.types.Numeric;
import net.sourceforge.cilib.type.types.Real;

/**
 * Step Ellipsoidal function as specified in reference.
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
public class StepEllipsoidal implements ContinuousFunction {
    private RotatedFunctionDecorator rotatedElliptic;

    public StepEllipsoidal() {
        Elliptic elliptic = new Elliptic();
        elliptic.setConditionNumber(100);
        this.rotatedElliptic = new RotatedFunctionDecorator();
        this.rotatedElliptic.setFunction(elliptic);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        Vector zBar = input.map(new F<Numeric, Numeric>() {
            @Override
            public Numeric f(Numeric a) {
                double z = a.doubleValue();
                double floor = Math.floor(0.5 + z);
                return z > 0.5 ? Real.valueOf(floor) : Real.valueOf(floor / 10.0);
            }
        });

        return 0.1 * Math.max(Math.abs(input.doubleValueOf(0)) / 1E4, rotatedElliptic.apply(zBar));
    }
}
