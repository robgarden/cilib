/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.pso.iterationstrategies;

import net.sourceforge.cilib.entity.Topology;
import net.sourceforge.cilib.pso.PSO;
import net.sourceforge.cilib.pso.velocityprovider.VelocityProvider;
import net.sourceforge.cilib.pso.velocityprovider.StandardVelocityProvider;
import net.sourceforge.cilib.pso.particle.Particle;
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
public class ARPSOIterationStrategy extends SynchronousIterationStrategy {
    protected boolean attracting;
    protected Measurement<Real> diversityMeasure;
    protected ControlParameter minDiversity;
    protected ControlParameter maxDiversity;
    protected VelocityProvider attractionProvider;
    protected VelocityProvider repulsionProvider;

    public ARPSOIterationStrategy() {
        attracting = true;
        diversityMeasure = new Diversity();
        ((Diversity)diversityMeasure).setNormalisationParameter(new DiagonalSpaceNormalisation());
        minDiversity = ConstantControlParameter.of(5e-1);
        maxDiversity = ConstantControlParameter.of(0.25);
        attractionProvider = new StandardVelocityProvider();
        repulsionProvider = new StandardVelocityProvider(
            ConstantControlParameter.of(0.729844),
            ConstantControlParameter.of(-1.496180),
            ConstantControlParameter.of(-1.496180));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ARPSOIterationStrategy getClone() {
        return this;
    }

    /**
     * Before performing the {@link SynchronousIterationStrategy}, determine
     * which phase the algorithm is currently in (attraction/repulsion) by
     * examining the diversity of the swarm. If there is a phase change, update
     * the {@link VelocityProvider}s of each {@link Particle} accordingly.
     *
     * @param pso the {@link PSO} to have an iteration applied.
     */
    @Override
    public void performIteration(PSO pso) {
        if (switchPhase(pso)) {
            VelocityProvider vp = attracting ? attractionProvider : repulsionProvider;

            for (Particle current : pso.getTopology()) {
                current.setVelocityProvider(vp);
            }
        }
        super.performIteration(pso);
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
        if (attracting && diversity < minDiversity.getParameter()) {
            attracting = false;
            return true;
        } else if (!attracting && diversity > maxDiversity.getParameter()) {
            attracting = true;
            return true;
        }
        return false;
    }
}
