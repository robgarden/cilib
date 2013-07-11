/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.measurement.functionmetric;

import fj.data.List;
import fj.data.Option;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.measurement.Measurement;
import net.sourceforge.cilib.type.types.container.StructuredType;
import net.sourceforge.cilib.type.types.Real;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;

/**
 * The standard deviation gradient between all points in the sample.
 */
public class StandardDeviationGradient implements Measurement<Real> {
    protected Option<ControlParameter> stepSize;
    protected AverageGradient averageGradient;

    public StandardDeviationGradient() {
        stepSize = Option.none();
        averageGradient = new AverageGradient();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public StandardDeviationGradient getClone() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Real getValue(Algorithm algorithm) {
        GradientHelper helper = new GradientHelper(algorithm);
        List<Entity> points = helper.getPoints();

        double avgGradient = averageGradient.getValue(algorithm).doubleValue();
        double variance = 0;

        for (int i = 0; i < points.length() - 1; i++) {
            Entity e1 = points.index(i);
            Entity e2 = points.index(i + 1);

            double gradient = helper.gradient(e1, e2, stepSize);
            variance += Math.pow(avgGradient - Math.abs(gradient), 2);
        }

        return Real.valueOf(Math.sqrt(variance / (points.length() - 1)));
    }

    public void setStepSize(ControlParameter stepSize) {
        this.stepSize = Option.some(stepSize);
        averageGradient.setStepSize(stepSize);
    }
}
