/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.pso.velocityprovider;

import net.sourceforge.cilib.pso.particle.Particle;
import net.sourceforge.cilib.type.types.container.Vector;

public final class KPSOClusterWidthVelocityProvider implements VelocityProvider {

    private double width;

    public KPSOClusterWidthVelocityProvider() {
        this.width = 1;
    }

    public KPSOClusterWidthVelocityProvider(double width) {
        this.width = width;
    }

    /**
     * Copy constructor.
     * @param copy The object to copy.
     */
    public KPSOClusterWidthVelocityProvider(KPSOClusterWidthVelocityProvider copy) {
        this.width = copy.width;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public KPSOClusterWidthVelocityProvider getClone() {
        return new KPSOClusterWidthVelocityProvider(this);
    }

    @Override
    public Vector get(Particle particle) {
        return Vector.fill(2 * width, particle.getDimension());
    }

    public void setWidth(double width) {
        this.width = width;
    }
}
