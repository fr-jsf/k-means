import java.awt.{Color, Graphics}
import javax.swing.JPanel
import scala.collection.mutable.ListBuffer

class Graphic(private var model : ListBuffer[ListBuffer[ListBuffer[Double]]], private var clusters : ListBuffer[ListBuffer[Double]]) extends JPanel {

  private final val xMAX : Int = rechercheMaxX()
  private final val yMAX = rechercheMaxY()
  private final val xMIN = rechercheMinX()
  private final val yMIN = rechercheMinY()

  def rechercheMaxX(): Int = {
    var max = 0.0
    for (i <- 0 until model.length) {
      for (points <- model(i)) {
        if (points(0) > max) {
          max = points(0)
        }
      }
    }
    max.toInt
  }

  def rechercheMaxY(): Int = {
    var max = 0.0
    for (i <- 0 until model.length) {
      for (points <- model(i)) {
        if (points(1) > max) {
          max = points(1)
        }
      }
    }
    max.toInt
  }

  def rechercheMinX(): Int = {
    var min = 3141592d
    for (i <- 0 until model.length) {
      for (points <- model(i)) {
        if (points(0) < min) {
          min = points(0)
        }
      }
    }
    min.toInt
  }

  def rechercheMinY(): Int = {
    var min = 3141592d
    for (i <- 0 until model.length) {
      for (points <- model(i)) {
        if (points(1) < min) {
          min = points(1)
        }
      }
    }
    min.toInt
  }

  override def paintComponent(g : Graphics) : Unit  = {
    super.paintComponent(g)
    setBackground(Color.WHITE)
    for (i <- 0 until model.length) {
      for (points <- model(i)) {
        drawCircle(g, points(0), points(1), chooseColor(i))
      }
    }
    for (clus <- clusters) {
      drawRect(g, clus(0), clus(1), Color.BLACK)
    }
  }

  def chooseColor(i : Int) : Color = i%7 match {
    case 0 => Color.ORANGE
    case 1 => Color.BLUE
    case 2 => Color.RED
    case 3 => Color.GREEN
    case 4 => Color.MAGENTA
    case 5 => Color.LIGHT_GRAY
    case 6 => Color.PINK
    case _ => Color.BLACK
  }

  def setClusters(c : ListBuffer[ListBuffer[Double]]): Unit = {
    this.clusters = c
  }

  def setModel(m :  ListBuffer[ListBuffer[ListBuffer[Double]]]): Unit = {
    this.model = m
  }

  def drawRect(g : Graphics, x : Double, y : Double, color : Color) : Unit = {
    val xScreen = this.getSize().width
    val yScreen = this.getSize().height
    g.setColor(color)
    val xPos = ((x - xMIN) / (xMAX) * (xScreen)).toInt
    val yPos = ((((y - yMIN) - yMAX) / (yMAX) * (yScreen)).toInt).abs
    g.fillRect(xPos, yPos, 10, 10)
  }

  def drawCircle(g : Graphics, x : Double, y : Double, color : Color) : Unit = {
    val xScreen = this.getSize().width
    val yScreen = this.getSize().height
    g.setColor(color)
    val xPos = ((x - xMIN) / (xMAX) * (xScreen)).toInt
    val yPos = ((((y - yMIN) - yMAX) / (yMAX) * (yScreen)).toInt).abs
    g.fillOval(xPos, yPos, 10, 10)
  }

}
