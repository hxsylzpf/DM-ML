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

object Util {

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
    def printTree(root: Node): Unit = {
        deepFirstTraverse(root)
    }
}

object Executor extends App {
    val path = "C://Users//zzz0419//Desktop//a.txt"
    val transactions = Util.readFile(path)
    val minSupport = 2
    val itemCount = transactions.flatMap(t=>t.split(" ")).groupBy(item=>item).mapValues(v=>v.size).filter(x=>x._2>=minSupport)
    for ((k, v) <- itemCount)
        println(k + " : " + v)

    val itemPointer = new HashMap[String,ArrayBuffer[Node]]
    for(k <- itemCount.keys)
        itemPointer(k) = new ArrayBuffer[Node]

    val fpTree = new Node(null)
    transactions.foreach(t => {
        //println("transaction:"+t.mkString(" "))
        var root = fpTree
        t.split(" ").filter(x=>itemCount.contains(x)).sortBy(x => -itemCount(x)).foreach(item => {
            //println("item: "+item)
            val t = root.children.get(item)
            if (t.isEmpty) {
                val node = new Node(root)
                node.value = item
                root.children(item) = node
                itemPointer(item) += node
                root = node
            } else {
                t.get.counter += 1
                root = t.get
            }
        })
        //println(t.sortBy(x => -itemCount(x)).mkString(" "))
    })
    Util.printTree(fpTree)

    def genCondTransaction(n:Node): Array[String] = {
        var p = n.parent
        val res = new ArrayBuffer[String]
        while (!p.isRoot) {
            res += p.value
            p = p.parent
        }
        res.toArray
    }
    val pointers = itemPointer("I5")
    val conTransactions = pointers.flatMap(p => genCondTransaction(p)).groupBy(x => x).mapValues(v=>v.size).filter(x=>x._2>=minSupport)

}