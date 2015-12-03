import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap, Stack}
import scala.io.Source
import scala.util.Random

class Node(val parent: Node) {
    var value: String = _
    val children: HashMap[String,Node] = new HashMap
    var counter = 1
    def isRoot: Boolean = parent == null
}

object Util {

    def constructTransactions(path: String): Array[String] = {
        val reader = Source.fromFile(path)
        val lines = reader.getLines.toArray
        val transactions = lines.map(line => {
          val items = line.split("::")
          (items(0),items(1))
        }).groupBy(x=>x._1).map(x=>{
        x._2.map(x=>x._2).mkString(" ")
      })
      transactions.toArray
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


class FPGrowth(val transactions:Array[String],val minSupport:Int) {

    val fpTree = new Node(null)
    val itemPointer = new HashMap[String,ArrayBuffer[Node]]

    def buildFPTree():Node = {
        // first traverse the database
        val itemCount = transactions
            .flatMap(x=>x.split(" "))
            .groupBy(item=>item)
            .mapValues(v=>v.size)
            .filter(x=>x._2>=minSupport)

        // initialize itemPointer
        for(k <- itemCount.keys)
            itemPointer(k) = new ArrayBuffer[Node]

        // second traverse the database
        transactions.foreach(t => {
            var root = fpTree
            t.split(" ")
                .filter(x=>itemCount.contains(x))
                .sortBy(x => -itemCount(x))
                .foreach(item => {
                val itemNode = root.children.get(item)
                if (itemNode.isEmpty) {
                    val node = new Node(root)
                    node.value = item
                    root.children(item) = node
                    itemPointer(item) += node
                    root = node
                } else {
                    itemNode.get.counter += 1
                    root = itemNode.get
                }
            })
        })
        fpTree
    }

    def genConditionalPatternBase(itemNodes:Array[Node]): Array[Array[(String,Int)]] = {
        val patterns = new ArrayBuffer[Array[(String,Int)]]()
        for (itemNode <- itemNodes) {
            var parent = itemNode.parent
            val path = new ArrayBuffer[(String,Int)]()
            while (!parent.isRoot) {
                path += Tuple2(parent.value,itemNode.counter)
                parent = parent.parent
            }
            patterns += path.toArray
        }
        patterns.toArray
    }

    def generatePattern(cands: Array[String]): Array[String] = {
        if (cands.size == 0)
            return Array()
        val fqPatterns = ArrayBuffer[String]()
        for (i <- 0 until cands.size) {
            val item = cands(i)
            fqPatterns += item
            for (rr <- generatePattern(cands.slice(i+1, cands.size))) {
                fqPatterns += (item + " " + rr)
            }
        }
        fqPatterns.toArray
    }

    def mineFrequentPattern(): Array[String] = {
        val freqPatterns = new ArrayBuffer[String]
        for(item <- itemPointer) {
            val condPatterns = this.genConditionalPatternBase(item._2.toArray)
            val cands = condPatterns.reduce(_ ++ _)
                                    .groupBy(item => item._1)
                                    .map(item => (item._1, item._2.size))
                                    .filter(item => item._2 >= minSupport)
                                    .map(item => item._1).toArray

            val partialPatterns = generatePattern(cands)
            if (partialPatterns.size == 0)
                freqPatterns += item._1
            else
                freqPatterns ++= partialPatterns.map(p => item._1 + " " + p)
        }
        freqPatterns.toArray
    }

}

object FPGrowth {
    def main(args: Array[String]) {
        val path = "/home/zzz/desktop/ml-10M100K/test.dat"
        val transactions = Util.constructTransactions(path)
        val minSupport = 2
        println("construction transactions finished!")

        val fpg = new FPGrowth(transactions,minSupport)
        val fpTree = fpg.buildFPTree()
        println("===========fp-tree=========")
        Util.printTree(fpTree)
        println()
    }
}
