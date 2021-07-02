import javax.swing.{BorderFactory, JButton, JLabel, SwingConstants}
import java.awt.{BorderLayout, Color, Font}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import swing._

object Window extends App {

  var k = new KMeans(10, 500)
  val window = new Frame()
  window.title = "kMeans - by Jonathan"
  window.size = new Dimension(1024,768)
  // window.resizable = false
  window.peer.setDefaultCloseOperation(EXIT_ON_CLOSE)
  window.visible = true
  window.centerOnScreen()

  // Ajout des composants
  var startButton = new JButton("Start kMeans")
  startButton.setFont(new Font("Verdana", Font.BOLD, 25))
  startButton.setBackground(new Color(34, 37, 38))
  startButton.setForeground(Color.WHITE)
  startButton.setFocusable(false)
  startButton.setBorder(BorderFactory.createEtchedBorder())

  var graph = new Graphic(k.pointsTab, k.mesClusters)
  var titre = new JLabel("Data before clustering", SwingConstants.CENTER)
  titre.setBackground(Color.WHITE)
  titre.setFont(new Font("Verdana", Font.BOLD, 25))
  titre.setOpaque(true)
  window.peer.getContentPane().add(titre, BorderLayout.NORTH)
  window.peer.getContentPane().add(graph, BorderLayout.CENTER)
  window.peer.getContentPane().add(startButton, BorderLayout.SOUTH)

  startButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      println("Lancement du KMeans")
      titre.setText("Data after clustering")
      k.lancerkmeans()
      graph.setModel(k.pointsTab)
      graph.setClusters(k.mesClusters)
      graph.repaint()
    }
  })

}