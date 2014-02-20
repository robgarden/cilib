/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.entity.topologies;

import net.sourceforge.cilib.clustering.KMeans;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.algorithm.AbstractAlgorithm;
import net.sourceforge.cilib.entity.comparator.AscendingFitnessComparator;
import net.sourceforge.cilib.entity.behaviour.Behaviour;
import net.sourceforge.cilib.pso.behaviour.StandardParticleBehaviour;
import net.sourceforge.cilib.pso.velocityprovider.KPSOClusterWidthVelocityProvider;

import fj.F;
import fj.F2;
import fj.data.List;
import java.util.ArrayList;
import java.util.Collections;

public class KMeansNeighbourhood<E extends Entity> extends Neighbourhood<E> {

	private KMeans kmeans;
	private List<E> unNiched;
	private ControlParameter c;
	private java.util.List<java.util.List<E>> clusters;
	private AscendingFitnessComparator ascending;
	private Behaviour nicheBehaviour;
	private Behaviour unNicheBehaviour;
	private Neighbourhood<E> vonNeumann;

	public KMeansNeighbourhood() {
		this.kmeans = new KMeans();
		this.clusters = null;
		this.c = ConstantControlParameter.of(10);
		this.ascending = new AscendingFitnessComparator();
		this.unNiched = List.list();
		this.vonNeumann = new VonNeumannNeighbourhood();
	}

    @Override
    public List<E> f(List<E> list, final E element) {
    	// re-cluster every c iterations
    	int iterations = AbstractAlgorithm.get().getIterations();
    	if ((this.clusters == null) || (iterations % (int)c.getParameter() == 0)) {
	    	clusters = kmeans.cluster(new ArrayList(list.toCollection()));

			for (java.util.List<E> cluster : clusters) {
				double width = 1;
				if (cluster.size() > 0) {
					width = Math.sqrt(KMeans.variance(cluster));
				}
				((KPSOClusterWidthVelocityProvider)((StandardParticleBehaviour)nicheBehaviour)
					.getVelocityProvider()).setWidth(width);
				for (E e : cluster) {
					e.setBehaviour(nicheBehaviour.getClone());
				}
			}

			// prune large clusters
			pruneClusters(list);

			// reinitialise unniched and change behaviour
			for (E e : unNiched) {
				e.reinitialise();
				e.setBehaviour(unNicheBehaviour);
			}
    	}

		// if unniched, return von neumann lattice
		if (unNiched.exists(new F<E, Boolean>() {
			@Override
			public Boolean f(E e) {
				return e.equals(element);
			}
		})) return vonNeumann.f(unNiched, element);

		// else return cluster neighbourhood
    	List<E> newList = list;

    	// find which cluster the particle is in
    	for (final java.util.List<E> c : clusters) {
    		if (c.contains(element)) {
    			// found the correct cluster,
				// now create neighbourhood containing all particles
				// in the same cluster
				newList = list.filter(new F<E, Boolean>() {
					@Override
					public Boolean f(E e) {
						return c.contains(e);
					}
				});
    		}
    	}

    	return newList;
    }

    private void pruneClusters(List<E> list) {
		unNiched = List.list();
    	int n = clusters.size();
		int numberOfParticles = 0;

		for (java.util.List<E> c : clusters) {
			numberOfParticles += c.size();
		}

		double average = (double)numberOfParticles / n;

		// prune large clusters
		for (java.util.List<E> c : clusters) {
			Collections.sort(c, ascending);
			while (c.size() > average) {
				unNiched = unNiched.snoc(c.remove(0));
			}
		}
    }

    public void setKmeans(KMeans kmeans) {
    	this.kmeans = kmeans;
    }

    public void setC(ControlParameter c) {
    	this.c = c;
    }

    public void setNicheBehaviour(Behaviour nicheBehaviour) {
    	this.nicheBehaviour = nicheBehaviour;
    }

    public void setUnNicheBehaviour(Behaviour unNicheBehaviour) {
    	this.unNicheBehaviour = unNicheBehaviour;
    }
}
