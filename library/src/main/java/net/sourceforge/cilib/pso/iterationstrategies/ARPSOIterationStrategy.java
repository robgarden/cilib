/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.pso.iterationstrategies;

import net.sourceforge.cilib.algorithm.population.AbstractIterationStrategy;
import net.sourceforge.cilib.algorithm.population.IterationStrategy;
import net.sourceforge.cilib.entity.Topology;
import net.sourceforge.cilib.pso.PSO;
import net.sourceforge.cilib.pso.particle.Particle;
import net.sourceforge.cilib.pso.particle.MultiBehaviorParticle;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.measurement.Measurement;
import net.sourceforge.cilib.measurement.single.diversity.Diversity;
import net.sourceforge.cilib.measurement.single.diversity.normalisation.DiagonalSpaceNormalisation;
import net.sourceforge.cilib.type.types.Real;

/**
 * Implementation of the attraction-repulsion (ARPSO) iteration strategy for PSO.
 * <p>
 * Reference:
 * <p>
 * R. Jacques, J.S. Vesterstrøm. "A diversity-guided particle swarm optimizer-
 * the ARPSO." Deptartment of Computer Science, University of Aarhus, Aarhus,
 * Denmark, Tech. Rep 2 (2002): 2002.
 */
public class ARPSOIterationStrategy extends AbstractIterationStrategy<PSO> {
    protected IterationStrategy<PSO> delegate;
    protected boolean attracting;
    protected Measurement<Real> diversityMeasure;
    protected ControlParameter minDiversity;
    protected ControlParameter maxDiversity;

    public ARPSOIterationStrategy() {
        delegate = new SynchronousIterationStrategy();
        attracting = true;
        diversityMeasure = new Diversity();
        ((Diversity)diversityMeasure).setNormalisationParameter(
            new DiagonalSpaceNormalisation());
        minDiversity = ConstantControlParameter.of(5e-6);
        maxDiversity = ConstantControlParameter.of(0.25);
    }

    public ARPSOIterationStrategy(ARPSOIterationStrategy copy) {
        delegate = copy.delegate.getClone();
        diversityMeasure = copy.diversityMeasure.getClone();
        minDiversity = copy.minDiversity.getClone();
        maxDiversity = copy.maxDiversity.getClone();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ARPSOIterationStrategy getClone() {
        return new ARPSOIterationStrategy(this);
    }

    /**
     * Before performing the delegate's {@link #performIteration}, determine
     * which phase the algorithm is currently in (attraction/repulsion) by
     * examining the diversity of the swarm. If there is a phase change, update
     * the {@link VelocityProvider}s of each {@link Particle} accordingly.
     *
     * @param pso the {@link PSO} to have an iteration applied.
     */
    @Override
    public void performIteration(PSO pso) {
        if (switchPhase(pso)) {
            for (Particle current : pso.getTopology()) {
                ((MultiBehaviorParticle)current).nextBehavior();
            }
        }
        delegate.performIteration(pso);
    }

    /**
     * Switch the phase of the algorithm based on the current phase and the
     * diversity of the swarm.
     *
     * @param pso   the {@link PSO} on which the diversity measurement is
     *              applied
     * @return      true if a phase change occured, false otherwise
     */
    private boolean switchPhase(PSO pso) {
        double diversity = diversityMeasure.getValue(pso).doubleValue();
        if ((attracting && diversity < minDiversity.getParameter())
            || (!attracting && diversity > maxDiversity.getParameter())) {

            attracting = !attracting;
            return true;
        }
        return false;
    }

    public void setMinDiversity(ControlParameter minDiversity) {
        this.minDiversity = minDiversity;
    }

    public void setMaxDiversity(ControlParameter maxDiversity) {
        this.maxDiversity = maxDiversity;
    }
}
