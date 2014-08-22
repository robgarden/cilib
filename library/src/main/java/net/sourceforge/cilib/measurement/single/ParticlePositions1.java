/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.measurement.single;

import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.measurement.Measurement;
import net.sourceforge.cilib.pso.PSO;
import net.sourceforge.cilib.pso.particle.Particle;
import net.sourceforge.cilib.type.types.StringType;
import net.sourceforge.cilib.type.types.container.Vector;

/**
 */
public class ParticlePositions1 implements Measurement<StringType> {
    private static final long serialVersionUID = -7891715753767819344L;

    /**
     * {@inheritDoc}
     */
    @Override
    public ParticlePositions1 getClone() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public StringType getValue(Algorithm algorithm) {
        final StringBuilder tmp = new StringBuilder();
        final PSO pso = (PSO) algorithm;
        for (Particle particle : pso.getTopology()) {
            tmp.append(particle.getPosition());
        }

        return new StringType(tmp.toString());
    }

}
