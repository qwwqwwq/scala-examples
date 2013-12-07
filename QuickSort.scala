import scala.io.Source

class QuickSorter(f: Int => Int => Boolean) {
    val comparator = f 
    def sort( x:List[Int] ) : List[Int] = {
	x match {
	    case x :: xs => 
		sort(xs.filter(comparator(x))) ++ List(x) ++ sort(xs.filterNot(comparator(x)))
	    case x => x
	}
    }
}

class ListReader( _fn:String ) {
    val fn = _fn
    def read() = {
	Source.fromFile("tmp").getLines.toList.map(x=>x.toInt)
    }
}

object QuickSort {
    def main(args: Array[String]) {
	println(args(0))
	def LessThan(x:Int)(y:Int):Boolean = { x < y }
	val sorter = new QuickSorter( LessThan )
	val reader = new ListReader(args(0))
	sorter.sort(reader.read).foreach(x => println(x))
    }
}
