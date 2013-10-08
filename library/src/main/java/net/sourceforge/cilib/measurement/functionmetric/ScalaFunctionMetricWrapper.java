/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.measurement.functionmetric;

import fj.F;
import fj.data.List;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.algorithm.population.HasTopology;
import net.sourceforge.cilib.measurement.Measurement;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.type.types.Numeric;
import net.sourceforge.cilib.type.types.Bounds;
import net.sourceforge.cilib.type.types.Real;
import scala.collection.JavaConversions;

/**
 * Dispersion function metric: Estimate presence of funnels
 */
public class ScalaFunctionMetricWrapper implements Measurement<Real> {
    protected FDCSearchability metric;

    /**
     * {@inheritDoc}
     */
    @Override
    public ScalaFunctionMetricWrapper getClone() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Real getValue(Algorithm algorithm) {
        List<Entity> entities = ((HasTopology)algorithm).getTopology();
        List<Point> points = entities.map(new F<Entity, Point>() {
            @Override
            public Point f(Entity e) {
              Vector pos = (Vector)e.getCandidateSolution();
              double[] newPos = new double[pos.size()];
              int i = 0;
              for (Numeric n : pos) {
                newPos[i++] = n.doubleValue();
              }

              return new Point(newPos, e.getFitness().getValue());
            }
        });

        return Real.valueOf(metric.apply(
          JavaConversions.asScalaIterator(points.iterator())));
    }

    public void setMetric(FDCSearchability metric) {
      this.metric = metric;
    }
}
