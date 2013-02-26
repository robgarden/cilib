/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import java.util.List;
import java.util.ArrayList;
import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.type.types.container.Matrix;
import net.sourceforge.cilib.util.Matrices;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.math.random.UniformDistribution;

/**
 * Gallagher's Gaussian Peaks function as specified in reference.
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
public class GallagherGaussianPeaks implements ContinuousFunction {
    private ControlParameter peaks;
    private List<Vector> y;
    private Vector w;
    private Vector c;
    private Matrix rotation;

    public GallagherGaussianPeaks() {
        this.peaks = ConstantControlParameter.of(21);
        this.y = new ArrayList<Vector>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        if (y.size() != input.size()) {
            initialise(input.size());
        }

        double max = Double.NEGATIVE_INFINITY;

        for (int i = 0; i < (int)peaks.getParameter(); i++) {
            double wi = w.doubleValueOf(i);
            double term = wi;

            if (term > max) {
                max = term;
            }
        }

        return irregular(Math.pow(10 - max, 2));
    }

    private double irregular(double x) {
        double xHat = x == 0.0 ? 0.0 : Math.log(Math.abs(x));
        double c1 = x > 0 ? 10 : 5.5;
        double c2 = x > 0 ? 7.9 : 3.1;
        return Math.signum(x) * Math.exp(xHat + 0.049 * (Math.sin(xHat * c1) + Math.sin(xHat * c2)));
    }

    /**
     * Initialise scaling, optima and conditioning.
     * @param size The size of the input vector.
     */
    private void initialise(int size) {
        UniformDistribution dist = new UniformDistribution();
        int numPeaks = (int)peaks.getParameter();

        this.y.clear();
        for (int i = 0; i < numPeaks; i++) {
            double bound = (i == 0) ? 4.0 : 4.9;
            Vector temp = Vector.fill(1, size);
            for (int j = 0; j < size; j++) {
                temp.setReal(j, dist.getRandomNumber(-bound, bound));
            }
            this.y.add(temp);
        }

        this.w = Vector.fill(1, numPeaks);
        this.w.setReal(0, 10.0);
        for (int i = 1; i < w.size(); i++) {
            this.w.setReal(i, 1.1 + 8 * ((i - 1.0) / numPeaks - 2));
        }

        Vector powerSet = Vector.fill(1, numPeaks - 1);
        for (int j = 0; j < powerSet.size(); j++) {
            powerSet.setReal(j, Math.pow(1000, 2 * (j / 19.0)));
        }
        powerSet = powerSet.permute();

        Vector a = Vector.fill(1, numPeaks);
        a.setReal(0, 1E6);
        for (int i = 1; i < powerSet.size(); i++) {
            a.setReal(i, powerSet.doubleValueOf(i));
        }

        this.c = Vector.fill(1, numPeaks);
        for (int i = 0; i < c.size(); i++) {
            double alpha = a.doubleValueOf(i);
            Vector condition = Vector.fill(1, c.size());
            for (int j = 0; j < condition.size(); j++) {
                condition.setReal(j, Math.pow(alpha, j * 0.5 / (condition.size() - 1)));
            }
            this.c.setReal(i, condition.doubleValueOf(i) / Math.pow(alpha, 0.25));
        }

        this.rotation = Matrices.getRandomOrthonormalMatrix(size);
    }

    /**
     * Sets the amount of peaks in the function.
     * @param peaks
     */
    public void setPeaks(ControlParameter peaks) {
        this.peaks = peaks;
    }
}
