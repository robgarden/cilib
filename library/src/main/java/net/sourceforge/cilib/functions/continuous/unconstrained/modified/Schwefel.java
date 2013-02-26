/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.functions.continuous.Penalty;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.math.random.UniformDistribution;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;

/**
 * Schwefel function as specified in reference.
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
public class Schwefel implements ContinuousFunction {
    private Vector randomSignVector;
    private Vector horizontalShift;
    private Penalty penalty;

    public Schwefel() {
        randomSignVector = Vector.of(0.0);
        penalty = new Penalty();
        penalty.setBoundary(ConstantControlParameter.of(5.0));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        if (randomSignVector.size() != input.size()) {
            initialise(input.size());
        }

        Vector xHat = randomSignVector.multiply(2).multiply(input);
        Vector zHat = Vector.fill(1, input.size());
        zHat.setReal(0, xHat.doubleValueOf(0));
        for (int i = 1; i < input.size(); i++) {
            double xi = xHat.doubleValueOf(i);
            zHat.setReal(i, xi + 0.25 * (xi - horizontalShift.doubleValueOf(i)));
        }

        Vector z = zHat.subtract(horizontalShift);
        for (int i = 0; i < z.size(); i++) {
            z.setReal(i, z.doubleValueOf(i) * Math.pow(10.0, i * 0.5 / z.size() - 1));
        }
        z.plus(horizontalShift);

        double sum = 0.0;

        for (int i = 0; i < input.size(); i++) {
            double zi = zHat.doubleValueOf(i);
            sum += zi * Math.sin(Math.sqrt(Math.abs(zi)))
                + 4.189828872724339;
        }

        return (-1.0 / input.size()) * sum + (100 * penalty.apply(z));
    }

    /**
     * Initialise the random sign vector and the horizontal shift
     * @param size The size of the input vector.
     */
    private void initialise(int size) {
        UniformDistribution dist = new UniformDistribution();
        randomSignVector = Vector.fill(1, size);
        horizontalShift = Vector.fill(1, size);
        for (int i = 0; i < size; i++) {
            double sign = 0.0;
            while (sign == 0) {
                sign = Math.signum(dist.getRandomNumber(-1.0, 1.0));
            }
            randomSignVector.setReal(i, sign);
            horizontalShift.setReal(i, (4.2096874633 / 2.0) * sign);
        }
    }
}
