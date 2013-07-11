/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.measurement.functionmetric;

import fj.data.List;
import fj.data.Option;
import com.google.common.base.Preconditions;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.algorithm.population.HasTopology;
import net.sourceforge.cilib.type.types.container.StructuredType;
import net.sourceforge.cilib.type.types.Numeric;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.util.distancemeasure.DistanceMeasure;
import net.sourceforge.cilib.util.distancemeasure.EuclideanDistanceMeasure;

/**
 * Gradient Helper: Calculate the gradient between points in a sample.
 *
 * If the step size is supplied, the distance between points does not need to 
 * be calculated. In this case, the way that points are initialised is important,
 * e.g. a Manhattan walk should be used to ensure a constant step size.
 */
public class GradientHelper {
    private double fitnessRange;
    private double domainRanges;
    private DistanceMeasure distance;
    private List<Entity> points;

    public GradientHelper(Algorithm algorithm) {
        this.points = ((HasTopology)algorithm).getTopology();
        Preconditions.checkArgument(points.length() >= 2,
            "Gradient measure requires at least 2 points in the sample");

        calculateFitnessRange(points);
        calculateDomainRanges(algorithm);
        this.distance = new EuclideanDistanceMeasure();
    }

    private void calculateFitnessRange(List<Entity> points) {
        double fmin = Double.MAX_VALUE;
        double fmax = Double.MIN_VALUE;
        for (Entity e : points) {
            double fitness = e.getFitness().getValue();
            fmax = Math.max(fmax, fitness);
            fmin = Math.min(fmin, fitness);
        }

        this.fitnessRange = fmax - fmin;
    }

    private void calculateDomainRanges(Algorithm algorithm) {
        double sumOfRanges = 0;
        StructuredType<Numeric> buildRep = algorithm.getOptimisationProblem()
            .getDomain().getBuiltRepresentation();
        for (Numeric n : buildRep) {
            sumOfRanges += n.getBounds().getRange();
        }

        this.domainRanges = sumOfRanges;
    }

    public double gradient(Entity e1, Entity e2, Option<ControlParameter> step) {
        StructuredType x1 = e1.getCandidateSolution();
        StructuredType x2 = e2.getCandidateSolution();
        double f1 = e1.getFitness().getValue();
        double f2 = e2.getFitness().getValue();

        double deltaY = (f2 - f1) / fitnessRange;

        double stepSize;
        if (step.isSome()) {
            stepSize = step.some().getParameter();
        } else {
            stepSize = distance.distance(e1.getCandidateSolution(), e2.getCandidateSolution());
        }
        double deltaX = stepSize / domainRanges;

        return deltaY / deltaX;
    }

    public List<Entity> getPoints() {
        return points;
    }
}
