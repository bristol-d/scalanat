import scala.scalajs.js.annotation.JSExportAll

/**
 * The result of a proof operation, in a JS-consumable form.
 */
@JSExportAll
case class JSResult(
    var success: Boolean,
    var message: String,
    var trace: Array[(Int, String)]
):
    def traceLength(): Int = trace.size
    def traceLine(index: Int): Int = trace(index)._1
    def traceValue(index: Int): String = trace(index)._2
