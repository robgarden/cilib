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
import net.sourceforge.cilib.type.types.container.TypeList;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.type.types.container.StructuredType;

public class Topology implements Measurement<TypeList> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Topology getClone() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TypeList getValue(Algorithm algorithm) {
        List<Entity> points = ((HasTopology)algorithm).getTopology();
        TypeList positions = new TypeList();
        for (Entity e : points) {
            Vector pos = (Vector)e.getCandidateSolution();
            pos.add(Real.valueOf(e.getFitness().getValue()));
            positions.add(pos);
        }
        return positions;
    }
}
