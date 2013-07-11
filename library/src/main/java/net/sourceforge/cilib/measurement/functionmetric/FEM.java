/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.measurement.functionmetric;

import fj.F;
import fj.data.List;
import com.google.common.base.Preconditions;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.algorithm.population.HasTopology;
import net.sourceforge.cilib.measurement.Measurement;
import net.sourceforge.cilib.type.types.Real;

/**
 * First Entropic Measure: Ruggedness of Landscape
 */
public class FEM implements Measurement<Real> {
    private List<Entity> points;

    /**
     * {@inheritDoc}
     */
    @Override
    public FEM getClone() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Real getValue(Algorithm algorithm) {
        this.points = ((HasTopology)algorithm).getTopology();
        Preconditions.checkArgument(this.points.length() >= 3, 
            "FEM measurement requires at least 3 points in the sample");

        double epsilonStar = infoStability();
        double increment = 0.05;
        int numEpsilons = (int)(1.0 / increment) + 1;
        double[] epsilon = new double[numEpsilons];

        double multiplier = 0;
        for (int e = 0; e < numEpsilons; e++) {
            epsilon[e] = epsilonStar * multiplier;
            multiplier += increment;
        }

        double[] H = new double[numEpsilons];
        double HMax = 0;
        for (int numE = 0; numE < numEpsilons; numE++) {
            H[numE] = infoContent(epsilon[numE]);
            HMax = Math.max(HMax, H[numE]);
        }

        return Real.valueOf(HMax);
    }

    private double infoContent(double e) {
        int[] s = calculateS(e);
        int[] num = new int[6];
        for (int i = 0; i < 6; i++) {
            num[i] = 0;
        }

        for (int p = 0; p < s.length - 1; p++) {
            int q = p + 1;
            if (s[p] != s[q]) {
                int hash = computeHash(s[p], s[q]);
                num[hash]++;
            }
        }

        int n = s.length - 1;
        double entropy = 0;
        for (int i = 0; i < 6; i++) {
            double prob = (double)num[i] / n;
            if (prob != 0) {
                entropy += prob * (Math.log(prob) / Math.log(6));
            }
        }

        return -entropy;
    }

    private int computeHash(int p, int q) {
        int x = -p + 2 * q;
        return x < 0 ? x + 3 : x + 2;
    }

    private double infoStability() {
        double epsilonBase = 10;
        double epsilonStep = 10;
        double epsilon = 0;
        int epsilonOrder = 0;
        boolean foundBigOrder = false;

        while (!foundBigOrder) {
            if (isLandscapeFlat(epsilon)) {
                foundBigOrder = true;
                epsilonStep /= epsilonBase;
            } else {
                epsilon = epsilonStep;
                epsilonOrder++;
                epsilonStep *= epsilonBase;
            }
        }

        double smallestStep = 0.01 * Math.pow(10, epsilonOrder);
        boolean notFoundPrecise = true;
        while (notFoundPrecise) {
            if (isLandscapeFlat(epsilon)) {
                if (epsilonStep <= smallestStep) {
                    notFoundPrecise = false;
                } else {
                    epsilon -= epsilonStep;
                    epsilonStep /= epsilonBase;
                    epsilon += epsilonStep;
                }
            } else {
                epsilon += epsilonStep;
            }
        }
        return epsilon;
    }

    private boolean isLandscapeFlat(double e) {
        int[] s = calculateS(e);
        for (int a : s) {
            if (a != 0) return false;
        }
        return true;
    }

    private int[] calculateS(double e) {
        int[] s = new int[points.length() - 1];

        for (int i = 1; i < points.length(); i++) {
            double fi = points.index(i).getFitness().getValue();
            double fi1 = points.index(i - 1).getFitness().getValue();

            if (fi - fi1 < -e) {
                s[i - 1] = -1;
            } else if (Math.abs(fi - fi1) <= e) {
                s[i - 1] = 0;
            } else {
                s[i - 1] = 1;
            }
        }

        return s;
    }
}
