package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

	val standardConfig = config(
			Key.exec.minWarmupRuns -> 5,
			Key.exec.maxWarmupRuns -> 10,
			Key.exec.benchRuns -> 10,
			Key.verbose -> true
			) withWarmer(new Warmer.Default)

			def main(args: Array[String]): Unit = {
		val radius = 3
				val width = 1920
				val height = 1080
				val src = new Img(width, height)
		val dst = new Img(width, height)
		val seqtime = standardConfig measure {
			VerticalBoxBlur.blur(src, dst, 0, width, radius)
		}
		println(s"sequential blur time: $seqtime ms")

		val numTasks = 3
		val partime = standardConfig measure {
			VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
		}
		println(s"fork/join blur time: $partime ms")
		println(s"speedup: ${seqtime / partime}")
	}

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

	/** Blurs the columns of the source image `src` into the destination image
	 *  `dst`, starting with `from` and ending with `end` (non-inclusive).
	 *
	 *  Within each column, `blur` traverses the pixels by going from top to
	 *  bottom.
	 */
	def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
			var height = src.height-1;
			for(i<-from until end){
				for(j<-0 to height){
					dst.update(i, j, scalashop.boxBlurKernel(src,i,j,radius));
				}
			}
	}

	/** Blurs the columns of the source image in parallel using `numTasks` tasks.
	 *
	 *  Parallelization is done by stripping the source image `src` into
	 *  `numTasks` separate strips, where each strip is composed of some number of
	 *  columns.
	 */
	def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
			if(numTasks==0){
				return	blur(src,dst,0,src.width,radius);
			}
			var divider = (src.width)/numTasks;
			if(divider==0){
  	     divider=1;
			}

			var range = (0 to src.width by divider);
			var list : List[Int] = range.toList;
			if(list.last!=src.width){
				list=(src.width::list.reverse).reverse
			}
			var subLists : List[(Int,Int)] = list.zip(list.tail);
				//			var array=Nil;
				//			blur(src,dst,subLists(0)._1,subLists(0)._2,radius);
				//			for( i<-0 to subLists.size-1){
				//			  var l = task(blur(src,dst,subLists(i)._1,subLists(i)._2,radius));
				//			  l::array;
				//			}

				var result : List[java.util.concurrent.ForkJoinTask[Unit]]=subLists.map(x=>task(blur(src,dst,x._1,x._2,radius)));
				result.map ( x => x.join() );		
	}


}
