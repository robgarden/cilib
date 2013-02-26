/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.functions.continuous.unconstrained.Weierstrass;

/**
 * Weierstrass function as specified in reference.
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
public class ModifiedWeierstrass implements ContinuousFunction {
    private Weierstrass weierstrass;

    public ModifiedWeierstrass() {
        this.weierstrass = new Weierstrass();
        this.weierstrass.setkMax(11);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        return 10 * Math.pow(weierstrass.apply(input) / input.size(), 3);
    }
}
