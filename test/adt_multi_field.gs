data Point {
    Point2D(Int, Int),
    Point3D(Int, Int, Int)
}

def sumPoint(p) = match p {
    Point2D(x, y) -> x + y,
    Point3D(x, y, z) -> x + y + z
}

def main() = putNumber(sumPoint(Point2D(10, 5)) + sumPoint(Point3D(3, 7, 17)))
