/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.pso.particle;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.base.Preconditions;
import java.util.List;
import java.util.Iterator;
import net.sourceforge.cilib.entity.EntityType;
import net.sourceforge.cilib.problem.Problem;
import net.sourceforge.cilib.problem.solution.Fitness;
import net.sourceforge.cilib.problem.solution.InferiorFitness;
import net.sourceforge.cilib.type.types.Int;
import net.sourceforge.cilib.type.types.container.Vector;

/**
 *
 */
public class MultiBehaviorParticle extends StandardParticle {
    protected List<ParticleBehavior> behaviors;
    protected int currentBehavior;

    /** Creates a new instance of MultiBehaviorParticle. */
    public MultiBehaviorParticle() {
        super();
        behaviors = Lists.newArrayList();
    }

    /**
     * Create a copy of the provided instance.
     * @param copy The instance to copy.
     */
    public MultiBehaviorParticle(MultiBehaviorParticle copy) {
        super(copy);
        behaviors = copy.behaviors;
        currentBehavior = copy.currentBehavior;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MultiBehaviorParticle getClone() {
       return new MultiBehaviorParticle(this);
    }

    public void addBehavior(ParticleBehavior behavior) {
        behaviors.add(behavior);
    }

    public void nextBehavior() {
        Preconditions.checkState(!behaviors.isEmpty(),
            "The list of particle behaviors cannot be empty.");

        currentBehavior++;
        if (currentBehavior == behaviors.size()) {
            currentBehavior = 0;
        }
        behavior = behaviors.get(currentBehavior);
    }
}
