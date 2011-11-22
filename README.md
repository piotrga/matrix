## Matrix creation:

$ val X = Matrix( (1,2,3),
$                (4,5,6),
$                (7,8,9))
$
$ val Y = Matrix( (1,2,3),
$                (4,5,6),
$                (7,8,9))
$
$ val y = Vector(1,2,3,4,5,6)
$ val z = RowVector(8,9,10,11)]]

## Matrix x Matrix operations:

[[scala> X*Y
res0: matrix.MatrixLike =
Matrix(3x3):
       30        36        42
       66        81        96
      102       126       150
  ]]

[[scala> X+Y
res1: matrix.MatrixLike =
Matrix(3x3):
        2         4         6
        8        10        12
       14        16        18
  ]]
[[scala> X-Y
res2: matrix.MatrixLike =
Matrix(3x3):
      0.0       0.0       0.0
      0.0       0.0       0.0
      0.0       0.0       0.0
  ]]

[[scala> X::Y
res3: matrix.Matrix =
Matrix(3x6):
        1         2         3         1         2         3
        4         5         6         4         5         6
        7         8         9         7         8         9
]]

## Operations on corresponding elements:

$ scala> X@*Y
$ res5: matrix.MatrixLike =
$ Matrix(3x3):
$         1         4         9
$        16        25        36
$        49        64        81

$ scala> X@/Y
$ res6: matrix.MatrixLike =
$ Matrix(3x3):
$         1         1         1
$         1         1         1
$         1         1         1

## Matrix x Scalar operations:

[[scala> 1/X
res4: matrix.MatrixLike =
Matrix(3x3):
        1       0.5  0.333333
     0.25       0.2  0.166667
 0.142857     0.125  0.111111
]]

scala> 2 * X
res8: matrix.MatrixLike =
Matrix(3x3):
        2         4         6
        8        10        12
       14        16        18

scala> 2*X == X*2
res3: Boolean = true

scala> 1+X
res10: matrix.MatrixLike =
Matrix(3x3):
        2         3         4
        5         6         7
        8         9        10


scala> 1+X == X+1
res2: Boolean = true

scala> 1-X
res4: matrix.MatrixLike =
Matrix(3x3):
      0.0        -1        -2
       -3        -4        -5
       -6        -7        -8

scala> X@^3
res7: matrix.MatrixLike =
Matrix(3x3):
        1         8        27
       64       125       216
      343       512       729

scala> 1 :: X
res12: matrix.Matrix =
Matrix(3x4):
        1         1         2         3
        1         4         5         6
        1         7         8         9

Other matrix operations:
------------------------

scala> X.dropFirstColumn
res14: matrix.MatrixLike =
Matrix(3x2):
        2         3
        5         6
        8         9

scala> X.T
res19: matrix.MatrixLike =
Matrix(3x3):
        1         4         7
        2         5         8
        3         6         9

scala> RowVector(1,2,3).T
res24: matrix.Vector =
Matrix(3x1):
        1
        2
        3

scala> Vector(1,2,3).T
res25: matrix.RowVector =
Matrix(1x3):
        1         2         3

scala> X.inverse
res21: matrix.Matrix =
Matrix(3x3):
-45035996 900719925 -45035996
900719925 -18014398 900719925
-45035996 900719925 -45035996

scala> diag(RowVector(1,2,3))
res18: matrix.Matrix =
Matrix(3x3):
        1       0.0       0.0
      0.0         2       0.0
      0.0       0.0         3

scala> ones(2,3)
res22: matrix.Matrix =
Matrix(2x3):
        1         1         1
        1         1         1



Check out https://github.com/piotrga/matrix/blob/master/src/test/scala/matrix/MatrixTest.scala for documentation.
