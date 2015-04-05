import org.scalajs.jquery._

import scala.scalajs.js
import scala.scalajs.js._

object HumanVsComputerApp extends JSApp {

  def main(): Unit = {
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    jQuery("#startGame").click(startGame _)
    jQuery("#input").keyup(validateInput _)
  }

  def validateInput(event: JQueryEventObject): Unit = {
    try {
      val code = jQuery("#input").value
      eval( s"""var f = ${code.toString};""")
      jQuery("#startGame").prop("disabled",false)
    }
    catch {
      case ex: Exception =>
        jQuery("#startGame").prop("disabled",true)

    }

  }

  def startGame(event: JQueryEventObject) = {
    println("Button pressed")
    val code = jQuery("#input").value
    println(s"Evaluating $code")
    val result = eval("f(117);")
    println(s"Result via eval = $result")
    val result2 = js.Dynamic.global.f(117)
    println(s"Result via js.Dynamic = $result2")
//    println(s"Apply function = ${result(117)}")
    jQuery("#output").text(result2.toString)
  }

  js.Dynamic.global.e

}
