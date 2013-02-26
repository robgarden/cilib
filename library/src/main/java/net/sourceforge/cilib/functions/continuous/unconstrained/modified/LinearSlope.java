/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;

/**
 * Linear Slope function as specified in reference.
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
public class LinearSlope implements ContinuousFunction {
    private ControlParameter horizontalShift;

    public LinearSlope() {
        this.horizontalShift = ConstantControlParameter.of(0.0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        double shift = horizontalShift.getParameter();
        double sum = 0;
        for (int i = 0; i < input.size(); i++) {
            double x = input.doubleValueOf(i);
            double z = shift * x < 25 ? x : shift;
            double s = Math.signum(shift) * Math.pow(10, i / (input.size() - 1));
            sum += 5 * Math.abs(s) - s * z;
        }
        return sum;
    }

    /**
     * Sets the horizontal shift.
     * @param horizontalShift
     */
    public void setHorizontalShift(ControlParameter horizontalShift) {
        this.horizontalShift = horizontalShift;
    }
}
