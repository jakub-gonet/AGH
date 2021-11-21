package test.agh.cs.lab2;

import main.agh.cs.lab2.Vector2d;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class Vector2dTest {
  @Test
  void equals_otherClass() {
    Assertions.assertNotEquals(new Object(), new Vector2d(1, 2));
  }

  @Test
  void equals_sameObj() {
    Vector2d v = new Vector2d(1, 2);
    Assertions.assertEquals(v, v);
  }

  @Test
  void equals_sameCoords() {
    Assertions.assertEquals(new Vector2d(1, 2), new Vector2d(1, 2));
  }

  @Test
  void equals_differentCoords() {
    Assertions.assertNotEquals(new Vector2d(1, 2), new Vector2d(3, 4));
  }

  @Test
  void toStringTest() {
    Assertions.assertEquals("(1,2)", new Vector2d(1, 2).toString());
  }

  @Test
  void precedes_preceding() {
    Vector2d v1 = new Vector2d(1, 2);
    Vector2d v2 = new Vector2d(0, 0);

    Assertions.assertTrue(v2.precedes(v1));
  }

  @Test
  void precedes_notPreceding() {
    Vector2d v1 = new Vector2d(1, 2);
    Vector2d v2 = new Vector2d(0, 0);

    Assertions.assertFalse(v1.precedes(v2));
  }

  @Test
  void precedes_equals() {
    Vector2d v1 = new Vector2d(1, 2);
    Vector2d v2 = new Vector2d(1, 2);

    Assertions.assertTrue(v2.precedes(v1));
  }

  @Test
  void follows_following() {
    Vector2d v1 = new Vector2d(1, 2);
    Vector2d v2 = new Vector2d(0, 0);

    Assertions.assertTrue(v1.follows(v2));
  }

  @Test
  void follows_notFollowing() {
    Vector2d v1 = new Vector2d(1, 2);
    Vector2d v2 = new Vector2d(0, 0);

    Assertions.assertFalse(v2.follows(v1));
  }

  @Test
  void follows_equals() {
    Vector2d v1 = new Vector2d(1, 2);

    Assertions.assertTrue(v1.follows(v1));
  }


  @Test
  void upperRight_basic() {
    Vector2d v1 = new Vector2d(-1, 1);
    Vector2d v2 = new Vector2d(3, -3);
    Vector2d v = new Vector2d(3, 1);

    Assertions.assertEquals(v, v1.upperRight(v2));
  }

  @Test
  void lowerLeft_basic() {
    Vector2d v1 = new Vector2d(-1, 1);
    Vector2d v2 = new Vector2d(3, -3);
    Vector2d v = new Vector2d(-1, -3);

    Assertions.assertEquals(v, v1.lowerLeft(v2));
  }


  @Test
  void add_positive() {
    Vector2d v1 = new Vector2d(1, 1);
    Vector2d v2 = new Vector2d(3, 3);
    Vector2d v = new Vector2d(4, 4);

    Assertions.assertEquals(v, v1.add(v2));
  }

  @Test
  void add_id() {
    Vector2d v0 = new Vector2d(0, 0);
    Vector2d v1 = new Vector2d(1, 1);

    Assertions.assertEquals(v1, v1.add(v0));
  }

  @Test
  void add_negative() {
    Vector2d v1 = new Vector2d(1, 1);
    Vector2d v2 = new Vector2d(-3, -5);
    Vector2d v = new Vector2d(-2, -4);

    Assertions.assertEquals(v, v1.add(v2));
  }

  @Test
  void subtract_positive() {
    Vector2d v1 = new Vector2d(1, 1);
    Vector2d v2 = new Vector2d(3, 5);
    Vector2d v = new Vector2d(-2, -4);

    Assertions.assertEquals(v, v1.subtract(v2));
  }

  @Test
  void subtract_id() {
    Vector2d v0 = new Vector2d(0, 0);
    Vector2d v1 = new Vector2d(1, 1);

    Assertions.assertEquals(v1, v1.subtract(v0));
  }

  @Test
  void subtract_negative() {
    Vector2d v1 = new Vector2d(1, 1);
    Vector2d v2 = new Vector2d(-3, -5);
    Vector2d v = new Vector2d(4, 6);

    Assertions.assertEquals(v, v1.subtract(v2));
  }

  @Test
  void opposite_positive() {
    Vector2d v = new Vector2d(4, 6);
    Vector2d v1 = new Vector2d(-4, -6);
    Assertions.assertEquals(v1, v.opposite());
  }

  @Test
  void opposite_id() {
    Vector2d v = new Vector2d(0, 0);
    Assertions.assertEquals(v, v.opposite());
  }

  @Test
  void opposite_negative() {
    Vector2d v = new Vector2d(-4, -6);
    Vector2d v1 = new Vector2d(4, 6);
    Assertions.assertEquals(v1, v.opposite());
  }
}
