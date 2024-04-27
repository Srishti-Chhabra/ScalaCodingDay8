import scala.collection.mutable.ArrayBuffer

class CourseScheduleI {
  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    val adjList = Array.fill[ArrayBuffer[Int]](numCourses)(ArrayBuffer.empty[Int])
    prerequisites.map{edge =>
      adjList(edge(1)) += edge(0)
    }
    val visited = Array.fill[Int](numCourses)(0)
    val pathVisited = Array.fill[Int](numCourses)(0)
    def dfs(node: Int): Boolean = {
      visited(node) = 1
      pathVisited(node) = 1
      def loop(i: Int): Boolean = {
        if(i>=adjList(node).size){
          pathVisited(node) = 0
          return false
        }
        if(visited(adjList(node)(i))==0){
          if(dfs(adjList(node)(i))==true)
            return true
        }
        else{
          if(pathVisited(adjList(node)(i))==1)
            return true
        }
        loop(i+1)
      }
      loop(0)
    }
    def loop(i: Int): Boolean = {
      if(i>=numCourses)
        return true
      if(visited(i)==0){
        if(dfs(i)==true)
          return false
      }
      loop(i+1)
    }
    loop(0)
  }
}
