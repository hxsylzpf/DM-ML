import java.io.PrintWriter
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class Node(val parent: Node) {
  var value: String = _
  val children: HashMap[String,Node] = new HashMap
  var counter = 1
  def isRoot: Boolean = parent == null
}

object FPUtil {

  def readFile(path: String): Array[String] = {
    val reader = Source.fromFile(path)
    val lines = reader.getLines.toArray
    lines
  }

  def writeFile(path: String): Unit = {
    val out = new PrintWriter(path)
    val rand = new Random
    for (i <- 1 to 50) {
        val t = ArrayBuffer[Int]()
        val m = rand.nextInt(9) + 1
        for (j <- 1 to m) {
            t += rand.nextInt(20)
        }
        out.println(t.mkString(" "))
    }
    out.close()
  }

  val stack: Stack[Node] = new Stack
  private def deepFirstTraverse(root: Node): Unit = {
    stack.push(root)
    if (root.children.size == 0) {
        println(stack.filter(item => !item.isRoot).map(item => item.value).mkString(" "))
        stack.pop
    } else {
        for (c <- root.children.values) {
            deepFirstTraverse(c)
        }
        stack.pop
    }
  }
  // 打印FP-tree
  def printTree(root: Node): Unit = {
    deepFirstTraverse(root)
  }

  // 沿结点向上得到根路径中的所有结点
  def genCondTransaction(n:Node): Array[String] = {
    var p = n.parent
    val res = new ArrayBuffer[String]
    while (!p.isRoot) {
      res += p.value
      p = p.parent
    }
    res.toArray
  }
}

class FPGrowth {

  val itemPointer = new HashMap[String,ArrayBuffer[Node]]     // 物品指针列表
  val root = new Node(null)                                  //  FP-tree的根结点

  // 构建FP-tree
  def buildFPTree(transactions: Array[String], minSupport: Int) {
    // 统计出频繁一项集
    val itemCount = transactions.flatMap(t=>t.split(" ")).groupBy(item=>item).mapValues(v=>v.size).filter(x=>x._2>=minSupport)
     for(k <- itemCount.keys)
      itemPointer(k) = new ArrayBuffer[Node]
    transactions.foreach(t => {
      var curNode = root
      t.split(" ").filter(x=>itemCount.contains(x)).sortBy(x => -itemCount(x)).foreach(item => {
        val t = curNode.children.get(item)
        if (t.isEmpty) {   // 新的路径
          val node = new Node(curNode)
          node.value = item
          curNode.children(item) = node
          itemPointer(item) += node
          curNode = node
        } else {
          t.get.counter += 1
          curNode = t.get
        }
      })
      //println(t.sortBy(x => -itemCount(x)).mkString(" "))
    })
  }
}

object Executor extends App {
  val path = "C://Users//zzz0419//Desktop//a.txt"
  val transactions = FPUtil.readFile(path)
  val minSupport = 2

  val model = new FPGrowth()
  model.buildFPTree(transactions, minSupport)
  FPUtil.printTree(model.root)
  val pointers = model.itemPointer("I5")
  val conTransactions = pointers.flatMap(p => FPUtil.genCondTransaction(p)).groupBy(x => x).mapValues(v=>v.size).filter(x=>x._2>=minSupport)

}