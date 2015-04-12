import org.scalajs.jquery._

import scala.scalajs.js

class ComputerPlayerInput(
                           inputElementId: String,
                           chooseMoveFunctionName: String,
                           inputValid: Boolean => Unit) {

  def setupUI() {
    jQuery(s"#$inputElementId").keyup(validateInput _)
    validateInput(null)
  }


  def validateInput(event: JQueryEventObject): Unit = {
    try {
      val code = jQuery(s"#$inputElementId").value
      js.eval(s"""var $chooseMoveFunctionName = ${code.toString};""")
      inputValid(true)
    }
    catch {
      case ex: Exception => inputValid(false)
    }
  }
}
