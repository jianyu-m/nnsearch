import java.io._

import scala.io.Source

/**
  * Created by max on 11/9/2017.
  */

case class Record(lat: Double, long: Double, loc: Int)

case class DRecord(record: Record, d: Double)

class NNpre(row: Int, col: Int) extends Serializable {

  var loc_set: Set[Int] = Set()

  var max_lat: Double = 0
  var min_lat: Double = 0

  var max_long: Double = 0
  var min_long: Double = 0

  var per_lat: Double = (max_lat - min_lat) / row

  var per_long: Double = (max_long - min_long) / col

  val grids: Array[Array[List[Record]]] = Array.ofDim[List[Record]](row, col)

  def load(): Unit = {

    var records: List[Record] = List()

    for (line <- Source.fromFile(NNpre.filename).getLines()) {
      val lines = line.split("\t")
      val loc = lines(4).toInt
      if (!loc_set.contains(loc)) {
        loc_set += loc
        records = Record(lines(2).toDouble, lines(3).toDouble, lines(4).toInt) :: records
      }
    }

    max_lat = records.head.lat
    min_lat = records.head.lat

    max_long = records.head.long
    min_long = records.head.long

    for (record <- records) {
      if (record.lat > max_lat)
        max_lat = record.lat
      else if (record.lat < min_lat)
        min_lat = record.lat

      if (record.long > max_long)
        max_long = record.long
      else if (record.long < min_long)
        min_long = record.long
    }

    per_lat = (max_lat - min_lat) / row

    per_long = (max_long - min_long) / col

    for (i <- 0 until row)
      for (j <- 0 until col)
        grids(i)(j) = List()

    for (record <- records) {
      var x = ((record.lat - min_lat) / per_lat).toInt
      var y = ((record.long - min_long) / per_long).toInt
      if (x == row)
        x = x - 1
      if (y == col)
        y = y - 1
      grids(x)(y) = record :: grids(x)(y)
    }

//    for (i <- 0 until row) {
//      val objectOutputStream = new ObjectOutputStream(new FileOutputStream(new File("data/row-" + i.toString + ".grid")))
//      objectOutputStream.writeObject(grids(i))
//      objectOutputStream.close()
//    }

    val objectOutputStream = new ObjectOutputStream(new FileOutputStream(new File("grids.data")))
    objectOutputStream.writeObject(this)
    objectOutputStream.close()

    records.take(10).foreach(println)

  }

  def grid_d(lat: Double, long: Double, k: Int, k_set_in:List[DRecord], q: Int, grid_records:List[Record]): (List[DRecord], Double, Double) = {
    var min_d = if(k_set_in.nonEmpty) k_set_in.last.d else Double.MaxValue
    var max_d:Double = if (k_set_in.nonEmpty) k_set_in.last.d else 0
    var k_set: List[DRecord] = k_set_in
    for (r <- grid_records) {
      if (r.loc != q) {
        val d = math.pow(r.lat - lat, 2) + math.pow(r.long - long, 2)
        if (d < min_d) {
          k_set = DRecord(r, d) :: k_set
          min_d = d
          if (d > max_d)
            max_d = d
          if (k_set.length > k) {
            k_set = k_set.dropRight(1)
            max_d = k_set.last.d
          }
        } else if (k_set.length < k) {
          if (d > max_d) {
            max_d = d
          }
          k_set = insert(k_set, DRecord(r, d))(DROrdering)
        } else if (d < max_d) {
          k_set = insert(k_set, DRecord(r, d))(DROrdering)
          k_set = k_set.dropRight(1)
          max_d = k_set.last.d
        }
      }
    }
    (k_set, min_d, max_d)
  }

  def query(q: Int, lat: Double, long: Double, k: Int): List[DRecord] = {
    var x = ((lat - min_lat) / per_lat).toInt
    var y = ((long - min_long) / per_long).toInt
    if (x == row)
      x = x - 1
    if (y == col)
      y = y - 1
    val grid_records = grids(x)(y)
    var q_r: Record = null
    for (r <- grid_records) {
      if (r.loc == q) {
        q_r = r
      }
    }

    if (q_r == null)
      List()
    else {
      var (k_set, min_d, max_d) = grid_d(lat, long, k, List(), q, grid_records)
      var access_cell_c = 1
      var running = true
      var point_list: List[(Int, Int)] = List((0, 0))
      var new_point_list: List[(Int, Int)] = List()
      while(running) {
        new_point_list = List()
        point_list.foreach(point => {
          val epses = NNpre.cell_direction.map(eps => (point._1 + eps._1, point._2 + eps._2)).filter(eps => eps._1 != 0 && eps._2 != 0)
          new_point_list = epses ::: new_point_list
          epses.foreach(eps => {
            val x_eps = eps._1
            val y_eps = eps._2
            if (x + x_eps >= 0 && x + x_eps <= row && y + y_eps >= 0 && y + y_eps <= col) {
              val x_d = if (x_eps >= 1) x_eps else 0
              val y_d = if (y_eps >= 1) y_eps else 0
              val low_d = math.pow((x + x_d) * per_lat - lat, 2) * x_eps + math.pow((y + y_d) * per_long - long, 2) * y_eps
              if (low_d < max_d) {
                // update
                access_cell_c += 1
                val m = grid_d(lat, long, k, k_set, q, grids(x + x_eps)(y + y_eps))
                k_set = m._1
                min_d = m._2
                max_d = m._3
              }
            }
          })
        })
        /* compute lower bounds of each cell */

        if (k_set.length >= k) {
          running = false
        }
        point_list = new_point_list
      }
      println("access " + access_cell_c)
      k_set
    }
  }

  def query_linear(q: Int, lat: Double, long: Double, k: Int): List[DRecord] = {
    var k_set: List[DRecord] = List()
    grids.foreach(grid => {
      grid.foreach(records => {
        val re = grid_d(lat, long, k, k_set, q, records)
        k_set = re._1
      })
    })
    k_set
  }

  def insert[DRecord](li:List[DRecord], x: DRecord)(implicit cmp: Ordering[DRecord]): List[DRecord] = {
    val (first, last) = li.partition(cmp.lteq(_, x))
    first ::: x :: last
  }

}

object DROrdering extends Ordering[DRecord] {
  override def compare(x: DRecord, y: DRecord): Int = x.d compare y.d
}

object NNpre {

  val cell_direction = List((1, 0), (-1, 0), (0, -1), (0, 1), (1, 1), (1, -1), (-1, 1), (-1, -1))

  val filename = "loc-gowalla_totalCheckins.txt"

  def loadNN(row: Int, col: Int): NNpre = {

    val NN = new NNpre(row, col)

    if (new File("grids.data").exists()) {
      val objectInputStream = new ObjectInputStream(new FileInputStream(new File("grids.data")))
      objectInputStream.readObject().asInstanceOf[NNpre]
    } else {
      val nn = new NNpre(row, col)
      nn.load()
      nn
    }
  }

  def main(args: Array[String]): Unit = {
    val nnpre = NNpre.loadNN(100, 100)
    println("indexing loaded.")
    while(true) {
      val input = readLine().split(" ")
      val lat = input(0).toDouble
      val long = input(1).toDouble
      val loc = input(2).toInt
      if (loc == -1) {
        return
      }
      nnpre.query(loc, lat, long, 5).foreach(println)
      println("linear search")
      nnpre.query_linear(loc, lat, long, 5).foreach(println)
    }
  }
}