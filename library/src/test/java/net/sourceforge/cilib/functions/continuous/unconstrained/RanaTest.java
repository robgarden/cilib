/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.unconstrained;

import net.sourceforge.cilib.type.types.container.Vector;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

public class RanaTest {

    private Rana function = new Rana();

    @Before
    public void instantiate() {
        this.function = new Rana();
    }

    /**
     * Test of evaluate method, of class {@link Rana}.
     */
    @Test
    public void testEvaluate() {
        Vector x = Vector.of(-512, -512);

        assertEquals(-511.708, function.f(x), 0.001);
    }
}
