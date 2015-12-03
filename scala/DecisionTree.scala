import javax.swing.tree.TreeNode

import scala.collection.mutable.{Stack, ArrayBuffer, HashMap}

class TreeNode {
  var featureIndex = 0                           // 这个树结点所对应的特征下标
	var label = ""                                // 预测类别
	val children = new HashMap[String, TreeNode]

	def isLeaf(): Boolean = children.size == 0
}

class DecisionTree(val root: TreeNode) {

  // 预测样本
	def predict(data: Array[String]): String = {
		var t = root
		while (!t.isLeaf()) {
			t = t.children(data(t.featureIndex))
		}
		t.label
	}
}

object DecisionTree {
	private def log2(x:Double) = math.log(x) / math.log(2.0)

  // 计算数据集合的熵
	def entropy(data:Array[(String,Array[String])]): Double = {
		val classCounters = data.groupBy(x=>x._1).mapValues(v=>v.size).values
		var impurity = 0.0
		val totalCount = classCounters.sum.toDouble
		for (classCount <- classCounters) {
			val freq = classCount / totalCount
			impurity -= freq*log2(freq)
		}
		impurity
	}

  // 数据集中的数据类别相同时停止
	def canStop(data:Array[(String,Array[String])]): Boolean = {
		data.groupBy(x => x._1).size == 1
	}

  // 寻找信息增益最大的特征
	def findBestSplit(data:Array[(String,Array[String])], attrIndices:Array[Int]): Int = {
		val bestSplitIndex = attrIndices.map(i => {
			var infoGain = DecisionTree.entropy(data.toArray)
			data.groupBy(item => item._2(i)).foreach(x => {
				val n = data.size.toDouble
				val d = x._2.size.toDouble
				infoGain -= d/n * DecisionTree.entropy(x._2.toArray)
			})
			//println("info gain: " + infoGain)
			(i,infoGain)
		}).maxBy(item => item._2)._1
		bestSplitIndex
	}

  // 构建决策树
	def buildTree(data:Array[(String,Array[String])], attrIndices:Array[Int]): TreeNode = {
		if (attrIndices.size == 0) {
			val root = new TreeNode
			root.label = data.groupBy(x=>x._1).mapValues(x=>x.size).maxBy(x=>x._2)._1
			return root
		}
		if (canStop(data)) {
			val root = new TreeNode
			root.label = data(0)._1
			return root
		} else {
			val root = new TreeNode
			root.featureIndex = findBestSplit(data,attrIndices)
			data.groupBy(x=>x._2(root.featureIndex)).foreach(x=>{
				root.children(x._1) = buildTree(x._2,attrIndices.filter(i=>i!=root.featureIndex))
			})
			return root
		}
	}
}

object DecisionTreeUtil {
  // 深度优先搜索
  private def deepFirstTraverse(root: TreeNode): Unit = {
    val stack: Stack[TreeNode] = new Stack()
    stack.push(root)
    if (root.children.size == 0) {
      println(stack.filter(item => !item.isLeaf()).map(item => item.featureIndex).mkString(" ")+": "+stack.pop.label)
    } else {
      for (c <- root.children.values) {
        deepFirstTraverse(c)
      }
      stack.pop
    }
  }
  // 打印决策树模型
  def print(root: TreeNode): Unit = {
    deepFirstTraverse(root)
  }
}

object DecisionTreeMain extends App {
	val data = new ArrayBuffer[(String,Array[String])]
	data.append(("N",Array("young","N","N","ok")))
	data.append(("N",Array("young","N","N","good")))
	data.append(("Y",Array("young","Y","N","good")))
	data.append(("Y",Array("young","Y","Y","ok")))
	data.append(("N",Array("young","N","N","ok")))
	data.append(("N",Array("medium","N","N","ok")))
	data.append(("N",Array("medium","N","N","good")))
	data.append(("Y",Array("medium","Y","Y","good")))
	data.append(("Y",Array("medium","N","Y","great")))
	data.append(("Y",Array("medium","N","Y","great")))
	data.append(("Y",Array("old","N","Y","great")))
	data.append(("Y",Array("old","N","Y","good")))
	data.append(("Y",Array("old","N","N","good")))
	data.append(("Y",Array("old","N","N","great")))
	data.append(("N",Array("old","N","N","ok")))

	val decisionTree = new DecisionTree(DecisionTree.buildTree(data.toArray, Array(0,1,2,3)))
  DecisionTreeUtil.print(decisionTree.root)
	val predictLabel = decisionTree.predict(Array("old","Y","Y","good"))
	println(predictLabel)
}
