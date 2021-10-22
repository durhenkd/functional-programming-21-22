

{-
    E 2.8.5: Using the declarations for Point and Shape2D write a function pointInShape , which
        determines if a given point is inside a given shape.
-}

heron : Float -> Float -> Float -> Float
heron a b c =
   let
     s = (a + b + c) / 2
   in
     sqrt (s * (s - a) * (s - b) * (s - c))
   
type alias Point = {x: Float, y: Float}

type Shape2D
    = Circle {center: Point, radius: Float}
    | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
    | Triangle {pointA: Point, pointB: Point, pointC: Point}

pointInShape : Point -> Shape2D -> Bool
pointInShape point shape = 
    let
        distance {x1, y1} {x2, y2} = sqrt((x1 - x2)^2 + (y1 - y2)^2)
        area point1 point2 point3 = heron (distance {x1 = point1.x, y1 = point1.y} {x2 = point2.x, y2 = point2.y}) 
                                            (distance {x1 = point1.x, y1 = point1.y} {x2 = point3.x, y2 = point3.y}) 
                                            (distance {x1 = point3.x, y1 = point3.y} {x2 = point2.x, y2 = point2.y})
    in
        case shape of
           Circle {center, radius} -> distance {x1 = point.x, y1 = point.y}  {x2 = center.x, y2 = center.x} < radius 
           Rectangle {topLeftCorner, bottomRightCorner} -> point.x > topLeftCorner.x && point.y < topLeftCorner.y && point.x < bottomRightCorner.x && point.y > bottomRightCorner.y
           Triangle {pointA, pointB, pointC} -> (area pointA pointB pointC) == ((area point pointA pointB) + (area point pointA pointC) + (area point pointB pointC))


{-

    > pointInShape (Point 1 1) (Circle {center = (Point 0 0), radius = 2})
    True : Bool
    > pointInShape (Point 1 1) (Circle {center = (Point 0 0), radius = 1})
    False : Bool

    > pointInShape (Point 0.5 0.3) (Rectangle {topLeftCorner = (Point -1 1), bottomRightCorner = (Point 1 -1)})
    True : Bool
    > pointInShape (Point 0.5 0.3) (Rectangle {topLeftCorner = (Point -1 1), bottomRightCorner = (Point 0 -1)})
    False : Bool


-}

