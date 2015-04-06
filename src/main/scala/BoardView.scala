import org.scalajs.jquery._

class BoardView {
  val cellValue = "-XO"

  def update(state: Seq[Int]) = {
    for ((player, i) <- state.zipWithIndex) {
      jQuery(s"#cell$i").text(cellValue(player).toString)
    }
  }
}
