/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.algorithm.population;

import net.sourceforge.cilib.algorithm.Algorithm;

/**
 * Implementation of the null iteration strategy which does nothing.
 */
public class NullIterationStrategy extends AbstractIterationStrategy {

    /**
     * {@inheritDoc}
     */
    @Override
    public NullIterationStrategy getClone() {
        return this;
    }

    /**
     * Don't perform any iteration
     */
    @Override
    public void performIteration(final Algorithm algorithm) {
        // do nothing
    }
}
