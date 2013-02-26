/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained.modified;

import fj.F;
import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.functions.continuous.decorators.IrregularFunctionDecorator;
import net.sourceforge.cilib.type.types.Numeric;
import net.sourceforge.cilib.type.types.Real;


/**
 * Attractive Sector function as specified in reference.
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
public class AttractiveSector implements ContinuousFunction {
    private ControlParameter horizontalShift;
    private F<Numeric, Numeric> irregularMapping;

    public AttractiveSector() {
        this.horizontalShift = ConstantControlParameter.of(0.0);
        this.irregularMapping = (new IrregularFunctionDecorator()).getMapping();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Double apply(Vector input) {
        double shift = horizontalShift.getParameter();
        double sum = 0;
        for (int i = 0; i < input.size(); i++) {
            double z = input.doubleValueOf(i);
            double s = z * shift > 0 ? 100 : 1;
            sum += z * s * z * s;
        }
        
        return irregularMapping.f(Real.valueOf(Math.pow(sum, 0.9))).doubleValue();
    }

    /**
     * Sets the horizontal shift.
     * @param horizontalShift
     */
    public void setHorizontalShift(ControlParameter horizontalShift) {
        this.horizontalShift = horizontalShift;
    }
}
