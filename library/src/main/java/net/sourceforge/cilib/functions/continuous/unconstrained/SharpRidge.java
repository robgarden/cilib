/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained;

import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;

/**
 * <p>The Sharp Ridge Function</p
 *
 */
public class SharpRidge implements ContinuousFunction {

    Spherical sphere;

    public SharpRidge() {
        sphere = new Spherical();
    }
    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        return input.doubleValueOf(0) * input.doubleValueOf(0) 
            + 100 * Math.sqrt(sphere.apply(input.copyOfRange(1, input.size())));
    }
}
