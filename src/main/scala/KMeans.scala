import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random.{nextDouble, nextInt};


class KMeans(private val nbClusters : Int, private var nbPoints : Int) {

  var mesPoints: ListBuffer[ListBuffer[Double]] = ListBuffer() // Initialise mon tableau de points
  var mesClusters: ListBuffer[ListBuffer[Double]] = ListBuffer() // Initialise mon tableau de clusters
  var pointsTab : ListBuffer[ListBuffer[ListBuffer[Double]]] = ListBuffer() // Pour avoir des groupes de points (pour les couleurs)
  var co_max = 7 // Générer des points entre 0 et 6
  var continuer_kmeans = -1 // Pour itérer le kMeans


  // Initialise mes tableaux avec des valeurs

  initPoints() // SANS LES DONNEES DE IRIS.DATA
  //initPointsIris() // AVEC LES DONNEES DE IRIS.DATA
  initClusters() // INIT CLUSTERS

  def initPoints() : Unit = {
    for (i <- 0 to nbPoints) {
      var mesPoints_temp: ListBuffer[Double] = ListBuffer()
      for(j <- 1 to 2) {
        mesPoints_temp.append(nextInt(co_max) + nextDouble()) // Ajoute des valeurs aléatoires entre 0 et 6
      }
      mesPoints.append(mesPoints_temp)
    }
    pointsTab.append(mesPoints)
  }

  def initPointsIris() : Unit = {
    var i = 0
    val filename = "src/main/scala/iris.data"

    for (line <- Source.fromFile(filename).getLines; if line != "") {
      var mesPoints_temp: ListBuffer[Double] = ListBuffer()
      var t = line.split(",")
      mesPoints_temp.append(t(0).toDouble)
      mesPoints_temp.append(t(1).toDouble)
      i += 1
      mesPoints.append(mesPoints_temp)
    }
    pointsTab.append(mesPoints)
  }

  def initClusters() : Unit = {
    for(i <- 1 to nbClusters) {
      mesClusters.append(mesPoints(nextInt(mesPoints.length - 1)))
    }
  }


  def moyenneIris() : Array[Double] = {
    var moy = Array.ofDim[Double](2)
    for (i <- mesPoints) {
      moy(0) += i(0)
      moy(1) += i(1)
    }
    moy(0) /= mesPoints.length
    moy(1) /= mesPoints.length
    print(moy(0))
    print(moy(1))
    moy
  }

  // moyenneIris()

  def varianceIris(): Array[Double] = {
    var moy = moyenneIris()
    var vari = Array.ofDim[Double](2)
    for (i <- 0 until 2) {
      for (points <- mesPoints) {
        vari(i) += (points(i) - moy(i)) * (points(i) - moy(i))
      }
    }
    vari(0) /= mesPoints.length
    vari(1) /= mesPoints.length
    print(vari(0))
    print(vari(1))
    vari
  }

  // varianceIris()

  def ecartTypeIris() : Array[Double] = {
    var vari = varianceIris()
    var ecart = Array.ofDim[Double](2)
    for (i <- 0 until 2) {
      ecart(i) = math.sqrt(vari(i)).toDouble
    }
    print(ecart(0))
    print(ecart(1))
    ecart
  }

  // ecartTypeIris()


  def afficher() : Unit = {
    println("--------MES POINTS--------")
    for (i <- mesPoints) {
      print("|\t " + BigDecimal(i(0)).setScale(2, BigDecimal.RoundingMode.HALF_UP) + " \t | \t " + BigDecimal(i(1)).setScale(2, BigDecimal.RoundingMode.HALF_UP) + " \t |")
      println("\n--------------------------")
    }
  }

  // afficher() // Afficher mes points

  def calculer_distance_pts(tab_calc_temp : ListBuffer[ListBuffer[ListBuffer[Double]]]): Unit = {
    // Ici, je calcule la distance entre chaque point et chaque cluster pour trouver le plus court
    for(i <- 0 until mesPoints.length) {
      var indice_cluster = 0
      var ecart_cluster = 3141592d // INITIALISE AVEC UNE VALEUR GRANDE
      for(j <- 0 until mesClusters.length) {
        var racine = math.sqrt({var somme_tab = 0d; for(nEff<-0 to 2-1) somme_tab+=math.pow(mesPoints(i)(nEff)-mesClusters(j)(nEff),2); somme_tab})
        if (ecart_cluster > racine) {
          ecart_cluster = racine
          indice_cluster = j
        }
      }
      tab_calc_temp(indice_cluster).append(mesPoints(i))
    }
  }

  def recentrer(tab_calc_temp : ListBuffer[ListBuffer[ListBuffer[Double]]], recentrer_mesCluster : ListBuffer[ListBuffer[Double]]) = {
    for(i <- 0 until nbClusters) {
      var temp : ListBuffer[Double] = ListBuffer()
      for(temporaire <- 0 until 2) temp.append(0)
      for(j <- 0 until tab_calc_temp(i).size)
        for(nEff <- 0 until tab_calc_temp(i)(j).size)
          temp(nEff) += tab_calc_temp(i)(j)(nEff)
      recentrer_mesCluster.append(temp)
    }
    for(i <- 0 until nbClusters) {
      if (tab_calc_temp(i).size > 0)
        for(j <- 0 until recentrer_mesCluster(i).size) {
          recentrer_mesCluster(i)(j) = recentrer_mesCluster(i)(j) / tab_calc_temp(i).size
          if (recentrer_mesCluster(i)(j) != mesClusters(i)(j)) continuer_kmeans = continuer_kmeans + 1
        }
      mesClusters(i) = recentrer_mesCluster(i)
    }
  }

  def lancerkmeans() {
    var nbIterations = 1 // Pour calculer le nombre d'itérations
    while( continuer_kmeans != 0) {
      continuer_kmeans = 0
      var tab_calc_temp : ListBuffer[ListBuffer[ListBuffer[Double]]] = ListBuffer()
      for(i <- 0 until nbClusters) tab_calc_temp.append(ListBuffer());
      calculer_distance_pts(tab_calc_temp)
      var recentrer_mesCluster : ListBuffer[ListBuffer[Double]] = ListBuffer()
      recentrer(tab_calc_temp, recentrer_mesCluster)
      if (continuer_kmeans != 0) nbIterations += 1
      pointsTab = tab_calc_temp
    }
    println("Le KMeans a été itéré " + nbIterations + " fois.")
  }

}