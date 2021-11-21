package test.agh.cs.lab2;

import main.agh.cs.lab2.MapDirection;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class MapDirectionTest {

  @Test
  void next_seq() {
    Assertions.assertEquals(MapDirection.EAST.next(), MapDirection.SOUTH);
    Assertions.assertEquals(MapDirection.SOUTH.next(), MapDirection.WEST);
    Assertions.assertEquals(MapDirection.WEST.next(), MapDirection.NORTH);
    Assertions.assertEquals(MapDirection.NORTH.next(), MapDirection.EAST);
  }

  @Test
  void previous_seq() {
    Assertions.assertEquals(MapDirection.EAST.previous(), MapDirection.NORTH);
    Assertions.assertEquals(MapDirection.NORTH.previous(), MapDirection.WEST);
    Assertions.assertEquals(MapDirection.WEST.previous(), MapDirection.SOUTH);
    Assertions.assertEquals(MapDirection.SOUTH.previous(), MapDirection.EAST);
  }
}
