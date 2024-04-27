import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

class CourseScheduleII {
  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    val adjList = Array.fill[ArrayBuffer[Int]](numCourses)(ArrayBuffer.empty[Int])
    prerequisites.map{edge =>
      adjList(edge(1)) += edge(0)
    }
    val indegree = Array.fill[Int](numCourses)(0)
    adjList.map{list =>
      list.map{node =>
        indegree(node) += 1
      }
    }
    val queue = new Queue[Int]
    val nodesWithZeroIndeg = for{
      i <- 0 to numCourses-1
      if(indegree(i)==0)
    } yield i
    nodesWithZeroIndeg.map{node =>
      queue.enqueue(node)
    }
    def topoSort(ls: List[Int]): List[Int] = {
      if(queue.isEmpty)
        return ls
      val node = queue.dequeue()
      adjList(node).map{adjNode =>
        indegree(adjNode) -= 1
        if(indegree(adjNode)==0)
          queue.enqueue(adjNode)
      }
      topoSort(node::ls)
    }
    val topo = topoSort(List())
    if(topo.size == numCourses)
      return topo.reverse.toArray
    return Array()
  }
}
