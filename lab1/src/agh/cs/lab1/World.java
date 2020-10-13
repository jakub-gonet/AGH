package agh.cs.lab1;

import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class World {
  public static void run(String[] cmds) {
    System.out.println("Start");
    System.out.println(String.join(", ",
        Stream.of(cmds).
            map(World::interpretCmd)
            .filter(Objects::nonNull)
            .collect(Collectors.joining("\n"))));
    System.out.println("Stop");
  }

  public static String interpretCmd(String cmd) {
    Direction d;
    switch (cmd) {
      case "f":
        d = Direction.FORWARD;
        break;
      case "b":
        d = Direction.BACKWARD;
        break;
      case "r":
        d = Direction.RIGHT;
        break;

      case "l":
        d = Direction.LEFT;
        break;
      default:
        d = null;
    }
    return interpretCmd(d);
  }

  public static String interpretCmd(Direction cmd) {
    switch (cmd) {
      case FORWARD:
        return "Zwierzak idzie do przodu";
      case BACKWARD:
        return "Zwierzak idzie do tyłu";
      case RIGHT:
        return "Zwierzak skręca w prawo";
      case LEFT:
        return "Zwierzak skręca w lewo";
    }
    return null;
  }

  public static void main(String[] args) {
    System.out.println("Start");
    run(args);
    System.out.println("Koniec");
  }
}
