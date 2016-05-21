package eurosim

sealed trait GameResult {
  def points: Int
  def opponentPoints: Int
}
sealed trait GameResultNoDraw extends GameResult

case object Win extends GameResultNoDraw {
  val points = 3
  val opponentPoints = 0
}
case object Draw extends GameResult {
  val points = 1
  val opponentPoints = 1
}
case object Loss extends GameResultNoDraw {
  val points = 0
  val opponentPoints = 3
}
