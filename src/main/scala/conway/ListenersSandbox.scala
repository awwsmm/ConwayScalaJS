package conway

import org.scalajs.dom
import org.scalajs.dom.html.Div
import org.scalajs.dom.{document, window}
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.Random

object ListenersSandbox {

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI()
    })
  }

  def setupUI(): Unit = {

    val container = div(
      alignItems := "center",
      display := "flex",
      justifyContent := "center",
      width := "100%",
      height := "100%"
    )

    val renderedContainer = container(dimensionsDiv())(
      div(
        position := "absolute",
        top := 0,
        left := 0,
        wordWrap := "break-word",
        width := 500
      )).render

    document.body.appendChild(renderedContainer)

    window.addEventListener("resize", { (e: dom.Event) =>
      renderedContainer.replaceChild(dimensionsDiv().render, renderedContainer.firstChild)
    })

    document.addEventListener("click", { (e: dom.MouseEvent) =>
      val x = e.clientX
      val y = e.clientY
      val nSteps = 100

      val maxRadius: Double = 10 + scala.util.Random.nextInt(91)
      val deltaMillis = maxRadius * 15 / nSteps
      val deltaRadius = maxRadius / nSteps
      val color = randomHexCode()

      val child = rendered(x, y, 0, color)
      document.body.appendChild(child)

      resize(child, x, y, Math.ceil(deltaRadius), 0, maxRadius, deltaRadius, deltaMillis, color)
    })

    document.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      val key = e.key match {
        case x if x == " " => "&nbsp;"
        case x => x
      }
      println(s"keydown: $key")

      renderedContainer.children(1).innerHTML += s"<span style='color:green'>$key</span>"
    })

    document.addEventListener("keyup", (e: dom.KeyboardEvent) => {
      val key = e.key match {
        case x if x == " " => "&nbsp;"
        case x => x
      }
      println(s"keyup: $key")

      renderedContainer.children(1).innerHTML += s"<span style='color:red'>$key</span>"
    })

  }

  def dimensionsDiv(): JsDom.TypedTag[Div] = {
    val (ww, wh) = innerDimensions()
    val area = ww * wh // 1029600
    val fs = (72 * area / 1029600).toInt
    div(fontSize := s"${fs}pt")(s"$ww x $wh = $area")
  }

  def innerDimensions(): (Double, Double) = {
    (dom.window.innerWidth, dom.window.innerHeight)
  }

  def rendered(x: Double, y: Double, r: Int, hexColor: String): Div = div(
    position := "absolute",
    top := y - r/2,
    left := x - r/2,
    width := r,
    height := r,
    backgroundColor := hexColor,
    borderRadius := r
  ).render

  def resize(
              child: Div,
              x: Double, y: Double,
              currentRadius: Double, previousRadius: Double, maxRadius: Double, deltaRadius: Double,
              deltaMillis: Double,
              color: String
            ): Unit = {

    def replace(delta: Double): Unit = {
      val newChild = rendered(x, y, Math.ceil(currentRadius + delta).toInt, color)

      window.setTimeout(() => {
        document.body.replaceChild(newChild, child)
        resize(newChild, x, y, currentRadius + delta, currentRadius, maxRadius, deltaRadius, deltaMillis, color)
      }, deltaMillis)
    }

    // done shrinking -- quit
    if (currentRadius < 0) document.body.removeChild(child)

    // begin shrinking
    else if (currentRadius >= maxRadius) replace(-deltaRadius)

    // growing
    else if (currentRadius > previousRadius) replace(deltaRadius)

    // shrinking
    else if (currentRadius < previousRadius) replace(-deltaRadius)

    else
      dom.window.alert(s"Error: tried to resize from $currentRadius (previously $previousRadius)")

  }


  def randomHexCode(): String = {
    val rand = new Random()
    val r = f"${rand.nextInt(256)}%02X"
    val g = f"${rand.nextInt(256)}%02X"
    val b = f"${rand.nextInt(256)}%02X"
    s"#$r$g$b"
  }


}