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
import net.sourceforge.cilib.util.distancemeasure.DistanceMeasure;
import net.sourceforge.cilib.util.distancemeasure.EuclideanDistanceMeasure;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.type.types.Numeric;
import net.sourceforge.cilib.type.types.Bounds;
import net.sourceforge.cilib.type.types.Real;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.util.selection.recipes.ElitistSelector;
import net.sourceforge.cilib.util.selection.Samples;

/**
 * Dispersion function metric: Estimate presence of funnels
 */
public class Dispersion implements Measurement<Real> {
    protected ControlParameter threshold;
    protected DistanceMeasure distanceMeasure;

    public Dispersion() {
        threshold = ConstantControlParameter.of(0.1);
        distanceMeasure = new EuclideanDistanceMeasure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Dispersion getClone() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Real getValue(Algorithm algorithm) {
        List<Entity> points = ((HasTopology)algorithm).getTopology();

        int amount = (int) (threshold.getParameter() * points.length());
        Preconditions.checkState(amount >= 2, "Dispersion metric requires at least 2 points in the sample.");

        // n best points in the sample
        java.util.List<Entity> bestPoints = new ElitistSelector().on(points).select(Samples.first(amount));

        // normalise n best points
        List<Vector> normPoints = List.iterableList(bestPoints).map(new F<Entity, Vector>() {
            @Override
            public Vector f(Entity e) {
                Vector position = (Vector) e.getCandidateSolution();

                return position.map(new F<Numeric, Numeric>() {
                    @Override
                    public Numeric f(Numeric a) {
                        // normalise numeric by upper and lower bounds
                        Bounds bounds = a.getBounds();
                        double norm = (a.doubleValue() - bounds.getLowerBound()) /
                        bounds.getRange();
                        return Real.valueOf(norm);
                    }
                });
            }
        });

        double distance = 0;
        int n = normPoints.length();
        int numDistances = (n * n - n) / 2;

        for (int i = 0; i < normPoints.length() - 1; i++) {
            for (int j = i + 1; j < normPoints.length(); j++) {
                distance += distanceMeasure.distance(normPoints.index(i),
                    normPoints.index(j));
            }
        }

        // average pairwise distance between best normalised points
        double average = distance / numDistances;
        int dimension = algorithm.getOptimisationProblem().getDomain().getDimension();
        return Real.valueOf(average - fullDispersion(dimension));
    }

    private double fullDispersion(int dimension) {
        return Math.sqrt(3 * dimension) / 4.0 - 0.1;
    }

    public void setThreshold(ControlParameter threshold) {
        Preconditions.checkArgument(threshold.getParameter() > 0 && threshold.getParameter() <= 1,
            "Dispersion threshold must be a percentage between 0 and 1.");
        this.threshold = threshold;
    }
}
