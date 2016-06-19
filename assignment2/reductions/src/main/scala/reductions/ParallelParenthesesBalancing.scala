package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

	@volatile var seqResult = false

			@volatile var parResult = false

			val standardConfig = config(
					Key.exec.minWarmupRuns -> 40,
					Key.exec.maxWarmupRuns -> 80,
					Key.exec.benchRuns -> 120,
					Key.verbose -> true
					) withWarmer(new Warmer.Default)

					def main(args: Array[String]): Unit = {
		val length = 100000000
				val chars = new Array[Char](length)
				val threshold = 10000
				val seqtime = standardConfig measure {
			seqResult = ParallelParenthesesBalancing.balance(chars)
		}
		println(s"sequential result = $seqResult")
		println(s"sequential balancing time: $seqtime ms")

		val fjtime = standardConfig measure {
			parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
		}
		println(s"parallel result = $parResult")
		println(s"parallel balancing time: $fjtime ms")
		println(s"speedup: ${seqtime / fjtime}")
	}
}

object ParallelParenthesesBalancing {

	/** Returns `true` iff the parentheses in the input `chars` are balanced.
	 */
	def balance(chars: Array[Char]): Boolean = {
			def  doBalance(chs : Array[Char], count : Int) : Boolean ={
					if(chs.isEmpty) return count == 0;
					if (count<0) return false;
					if(chs.head=='(') return doBalance(chs.tail,count+1);
					else if (chs.head==')') return doBalance(chs.tail,count-1);
					else return doBalance(chs.tail,count);

			}

			doBalance(chars,0);
	}

	/** Returns `true` iff the parentheses in the input `chars` are balanced.
	 */
	def parBalance(chars: Array[Char], threshold: Int): Boolean = {

			def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) :(Int,Boolean) ={
			  var start : Int = idx;
			  var right : Boolean = true;
			  var count : Int = 0;
			  while(start < until){
			   
			    if(chars(start)=='('){
			      count = count+1;
			    }
			    else if( chars(start)==')'){
			      count = count -1;
			    }
			     if(count <0){
			      right = false;
			    }
			    start=start+1;
			  }
			  (count,right);
			  
			}

			def reduce(from: Int, until: Int) :(Int,Boolean) = {
			 
			  if(until-from<=threshold){
			     println(from +","+until);
			    return traverse(from,until,0,0);
			  }
			  else{
			    var mid = from + (until-from)/2;
			    var((count1 : Int, right1 : Boolean), (count2 : Int, right2 : Boolean)) = parallel(reduce(from,mid),reduce(mid,until));
			    if(right1==false){
			      return(count1+count2,false);
			    }
			    else{
			      val least = count1+count2;

			      if(least == 0){
			        return (least , true);
			      }
			      else{
			        if(right2==false)
			        return (least , false);
			        else
			          return (least, true);
			      }
			    }
			  }
			}
				

	  reduce(0, chars.length)== (0,true);
	}

	// For those who want more:
	// Prove that your reduction operator is associative!

}
