package TypeParameterization

sealed trait Queue[T]

object Queue {
  def apply[T](xs: T*) =
}