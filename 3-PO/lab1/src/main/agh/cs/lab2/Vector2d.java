package main.agh.cs.lab2;

public class Vector2d {
  final int x;
  final int y;

  public Vector2d(int x, int y) {
    this.y = y;
    this.x = x;
  }

  public boolean precedes(Vector2d other) {
    return this.x <= other.x && this.y <= other.y;
  }

  public boolean follows(Vector2d other) {
    return this.x >= other.x && this.y >= other.y;
  }

  public Vector2d upperRight(Vector2d other) {
    return new Vector2d(other.x, this.y);
  }

  public Vector2d lowerLeft(Vector2d other) {
    return new Vector2d(this.x, other.y);
  }

  public Vector2d add(Vector2d other) {
    return new Vector2d(this.x + other.x, this.y + other.y);
  }

  public Vector2d subtract(Vector2d other) {
    return new Vector2d(this.x - other.x, this.y - other.y);
  }

  public Vector2d opposite() {
    return new Vector2d(-this.x, -this.y);
  }

  public boolean equals(Object other) {
    if (this == other)
      return true;
    if (!(other instanceof Vector2d))
      return false;
    Vector2d other_c = (Vector2d) other;

    return this.x == other_c.x && this.y == other_c.y;
  }

  @Override
  public String toString() {
    return "(" + x + "," + y + ")";
  }
}
