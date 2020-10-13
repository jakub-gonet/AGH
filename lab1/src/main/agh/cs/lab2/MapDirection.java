package main.agh.cs.lab2;

public enum MapDirection {
  NORTH, EAST, SOUTH, WEST;

  private static final MapDirection[] v = values();

  public MapDirection next() {
    return v[(this.ordinal() + 1) % v.length];
  }

  public MapDirection previous() {
    return v[(this.ordinal() - 1 + v.length) % v.length];
  }

  public Vector2d toUnitVector() {
    switch (this) {
      case NORTH:
        return new Vector2d(0, 1);
      case SOUTH:
        return new Vector2d(0, -1);

      case WEST:
        return new Vector2d(-1, 0);

      case EAST:
        return new Vector2d(1, 0);

      default:
        throw new IllegalArgumentException();
    }
  }

  @Override
  public String toString() {
    switch (this) {
      case NORTH:
        return "Północ";
      case SOUTH:
        return "Południe";
      case WEST:
        return "Zachód";
      case EAST:
        return "Wschód";
      default:
        throw new IllegalArgumentException();
    }
  }
}
