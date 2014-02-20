/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */

package net.sourceforge.cilib.clustering;

import net.sourceforge.cilib.controlparameter.ControlParameter;
import net.sourceforge.cilib.controlparameter.ConstantControlParameter;
import net.sourceforge.cilib.type.types.container.Vector;
import net.sourceforge.cilib.entity.Entity;
import net.sourceforge.cilib.math.random.generator.Rand;
import net.sourceforge.cilib.util.distancemeasure.DistanceMeasure;
import net.sourceforge.cilib.util.distancemeasure.EuclideanDistanceMeasure;

import java.util.List;
import java.util.ArrayList;

public class KMeans <E extends Entity> {
	private ControlParameter k;
	private ControlParameter rk;
	public static DistanceMeasure distance;

	/*
     * Default Constructor for DataClusteringPSO
     */
    public KMeans() {
    	k = ConstantControlParameter.of(5);
    	rk = ConstantControlParameter.of(10);
    	distance = new EuclideanDistanceMeasure();
    }

    /*
     * copy constructor for KMeans
     * @param copy Th KMeans to be copied
     */
    public KMeans(KMeans copy) {
    	this.k = copy.k.getClone();
    	this.rk = copy.rk.getClone();
    	this.distance = copy.distance;
    }

    /*
     * Clone method for the KMeans
     * @return new instance of the KMeans
     */
    public KMeans getClone() {
        return new KMeans(this);
    }

    public List<List<E>> run(List<E> topology) {
    	List<List<List<E>>> solutions = new ArrayList<List<List<E>>>();

    	for (int i = 0; i < rk.getParameter(); i++) {
    		solutions.add(cluster(topology));
    	}

    	double minScattering = scattering(solutions.get(0));
        List<List<E>> bestSolution = solutions.get(0);

    	for (List<List<E>> solution : solutions) {
    		double scattering = scattering(solution);
    		if (scattering < minScattering) {
    			minScattering = scattering;
    			bestSolution = solution;
    		}
    	}

	    return bestSolution;
    }

    public List<List<E>> cluster(List<E> topology) {
    	boolean newAssignments = true;

    	List<Vector> centroids = new ArrayList<Vector>();
    	List<Integer> assignments = new ArrayList<Integer>(topology.size());

    	for (int i = 0; i < topology.size(); i++) {
    		assignments.add(-1);
    	}

    	// choose k random centroids
    	List<Integer> indices = new ArrayList<Integer>();
    	for (int i = 0; i < k.getParameter(); i++) {
    		int index = Rand.nextInt(topology.size());
    		while (indices.contains(index)) {
    			index = Rand.nextInt(topology.size());
    		}
    		indices.add(index);

    		E entity = topology.get(index); 
    		centroids.add((Vector)entity.getPosition());
    	}

    	// assign each pattern to a centroid until no patterns change assignment
    	while (newAssignments) {
    		newAssignments = false;

    		for (int j = 0; j < topology.size(); j++) {
    			E e = topology.get(j);

    			double minDistance = Double.MAX_VALUE;
    			Vector nearest;
    			int index = -1;

    			for (int i = 0; i < centroids.size(); i++) {
    				Vector c = centroids.get(i);
    				double d = distance.distance((Vector)e.getPosition(), c);
    				if (d < minDistance) {
    					minDistance = d;
    					nearest = c;
    					index = i;
    				}	
    			}

    			if (index != assignments.get(j)) {
    				assignments.set(j, index);
    				newAssignments = true;
    			}
    		}

	    	// calculate new centroids
	    	for (int i = 0; i < centroids.size(); i++) {
	    		Vector newC = centroids.get(i).getClone();
	    		double count = 1;

	    		for (int j = 0; j < assignments.size(); j++) {
	    			if (assignments.get(j) == i) {
	    				newC = newC.plus((Vector)topology.get(j).getPosition());
	    				count++;
	    			}
	    		}

	    		centroids.set(i, newC.divide(count));
	    	}
    	}

    	// create centroid holder solution
        List<List<E>> solution = new ArrayList<List<E>>();

    	for (int i = 0; i < centroids.size(); i++) {
            List<E> cluster = new ArrayList<E>();

    		for (int j = 0; j < assignments.size(); j++) {
    			if (assignments.get(j) == i) {
                    cluster.add(topology.get(j));
    			}
    		}

    		solution.add(cluster);
    	}

    	return solution;
    }
    
    public double scattering(List<List<E>> clusters) {
		double scatter = 0;

		for (List<E> c : clusters) {
			scatter += Math.sqrt(variance(c));
		}

		return scatter;   	
    }

    public static <E extends Entity> double variance(List<E> cluster) {
    	double variance = 0;

        Vector c = centroid(cluster);

        for (E e : cluster) {
            double d = distance.distance(c, e.getPosition());
            variance += (d * d) / (cluster.size() - 1);
        }

    	return variance;
    }

    public static <E extends Entity> Vector centroid(List<E> cluster) {
        Vector centroid = (Vector)cluster.get(0).getPosition();
        int count = 1;

        for (E e : cluster) {
            centroid = centroid.plus((Vector)e.getPosition());
            count++;
        }

        return centroid.divide(count);
    }

	public void setK(ControlParameter k) {
		this.k = k;
	}

	public void setRk(ControlParameter rk) {
		this.rk = rk;
	}
}