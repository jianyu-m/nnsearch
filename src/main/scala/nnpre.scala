import java.io._

import scala.io.Source
import scala.util.Random

/**
  * Created by max on 11/9/2017.
  */

case class Record(lat: Double, long: Double, loc: Int)

case class DRecord(record: Record, d: Double)

class NNpre(val row: Int, val col: Int) extends Serializable {

  var loc_set: Set[Int] = Set()

  var max_lat: Double = 0
  var min_lat: Double = 0

  var max_long: Double = 0
  var min_long: Double = 0

  var per_sqrt: Double = (max_lat - min_lat) / row

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

    val per_lat = (max_lat - min_lat) / row

    val per_long = (max_long - min_long) / col

    per_sqrt = math.max(per_lat, per_long)

    for (i <- 0 until row)
      for (j <- 0 until col)
        grids(i)(j) = List()

    for (record <- records) {
      var x = ((record.lat - min_lat) / per_sqrt).toInt
      var y = ((record.long - min_long) / per_sqrt).toInt
      if (x == row)
        x = x - 1
      if (y == col)
        y = y - 1
      grids(x)(y) = record :: grids(x)(y)
    }

    val objectOutputStream = new ObjectOutputStream(new FileOutputStream(new File("grids.data")))
    objectOutputStream.writeObject(this)
    objectOutputStream.close()
    // records.take(10).foreach(println)
  }

  def grid_d(lat: Double, long: Double, k: Int, k_set_in:List[DRecord], grid_records:List[Record]): List[DRecord] = {
    var k_set: List[DRecord] = k_set_in
    for (r <- grid_records) {
      val d = math.pow(r.lat - lat, 2) + math.pow(r.long - long, 2)
      if (k_set.length < k) {
        k_set = insert(k_set, DRecord(r, d))(DROrdering)
      } else if (d < k_set.last.d) {
        k_set = insert(k_set, DRecord(r, d))(DROrdering)
        k_set = k_set.dropRight(1)
      }
    }
    k_set
  }

  def query(lat: Double, long: Double, k: Int): (List[DRecord], Int) = {
    var x = ((lat - min_lat) / per_sqrt).toInt
    var y = ((long - min_long) / per_sqrt).toInt
    if (x == row)
      x = x - 1
    if (y == col)
      y = y - 1
    val grid_records = grids(x)(y)
    var inserted = false

    var k_set = grid_d(lat, long, k, List(), grid_records)
    var access_cell_c = 1
    var running = true
    var point_list: List[(Int, Int)] = List((0, 0))
    var new_point_list: List[(Int, Int)] = List()
    var visited: Set[(Int, Int)] = Set()
    visited += ((0, 0))
    while(running) {
      new_point_list = List()
      inserted = false
      point_list.foreach(point => {
        val epses = NNpre.cell_direction.map(eps => (point._1 + eps._1, point._2 + eps._2)).filter(eps => {
          val has = !visited.contains(eps)
          if (!has) {
            visited += eps
          }
          has
        })
        new_point_list = epses ::: new_point_list
        epses.foreach(eps => {
          visited += eps
          val x_eps = eps._1
          val y_eps = eps._2
          if (x + x_eps >= 0 && x + x_eps < row && y + y_eps >= 0 && y + y_eps < col) {
            val x_d = if (x_eps < 0) x_eps + 1 else x_eps
            val y_d = if (y_eps < 0) y_eps + 1 else y_eps
            val x_nonzero = if (x_eps != 0) 1 else 0
            val y_nonzero = if (y_eps != 0) 1 else 0
            val low_d = math.sqrt(math.pow((x + x_d) * per_sqrt - lat + min_lat, 2) * x_nonzero + math.pow((y + y_d) * per_sqrt - long + min_long, 2) * y_nonzero)
            if (k_set.length < k || low_d < k_set.last.d) {
              // update
              inserted = true
              access_cell_c += 1
              k_set = grid_d(lat, long, k, k_set, grids(x + x_eps)(y + y_eps))
            }
          }
        })
      })
      /* compute lower bounds of each cell */
      if (k_set.length >= k && !inserted) {
        running = false
      }
      point_list = new_point_list
    }
    (k_set, access_cell_c)
  }

  def query_linear(lat: Double, long: Double, k: Int): List[DRecord] = {
    var k_set: List[DRecord] = List()
    grids.foreach(grid => {
      grid.foreach(records => {
        val re = grid_d(lat, long, k, k_set, records)
        k_set = re
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

  def correctness_test(n: Int, nn: NNpre, k: Int): Unit = {
    val random = new Random()
     for (i <- 1 to n) {
       val lat = random.nextDouble() * nn.row * nn.per_sqrt + nn.min_lat
       val long = random.nextDouble() * nn.col * nn.per_sqrt + nn.min_long
       if (lat < nn.max_lat && long < nn.max_long) {
         val grid_r = nn.query(lat, long, k)._1.map(_.d).sum
         val lin_r = nn.query_linear(lat, long, k).map(_.d).sum
         //       print(grid_r, lin_r)
         //       assert(grid_r <= lin_r + Double.MinValue * 10)
         println(grid_r)
         println(lin_r)
         if (grid_r >= lin_r + 0.1) {
           println("not good")
         }
         println()
       }
     }
  }

  def benchmark(n: Int, nn: NNpre, k: Int): Unit = {
    val random = new Random()
    var access_requests: List[(Double, Double)] = List()
    for (i <- 1 to n) {
      val lat = random.nextDouble() * nn.row * nn.per_sqrt + nn.min_lat
      val long = random.nextDouble() * nn.col * nn.per_sqrt + nn.min_long
      if (lat < nn.max_lat && long < nn.max_long) {
        access_requests ::= ((lat, long))
      }
    }
    println("total request " + access_requests.length)
    var total_access_cell = 0
    val s = System.nanoTime()
    for (r <- access_requests) {
      total_access_cell += nn.query(r._1, r._2, k)._2
    }
    val e = System.nanoTime()
    println("time: " + (e - s) / access_requests.length / 1000000 + "ms; cells: " +
      total_access_cell / access_requests.length )

    val s_l = System.nanoTime()
    for (r <- access_requests) {
      nn.query_linear(r._1, r._2, k)
    }
    val e_l = System.nanoTime()
    println("time: " + (e - s) / access_requests.length / 1000000 + "ms")
  }

  def main(args: Array[String]): Unit = {
    val nnpre = NNpre.loadNN(100, 100)
    println("indexing loaded.")
    benchmark(1000, nnpre, 5)
  }
}
