/**
 * This file is part of Cro - Closure reopening in Java
 *
 * Copyright (c) 2010 by Erik Søe Sørensen
 *
 * Licensed under the GNU Lesser General Public License (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.gnu.org/licenses/lgpl.html
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package cro;

import org.objectweb.asm.tree.{ClassNode, MethodNode, AbstractInsnNode,
			       LabelNode, InsnNode, IntInsnNode, VarInsnNode,
			       FieldInsnNode, TypeInsnNode, MethodInsnNode,
			       LineNumberNode
			     }
import org.objectweb.asm.{MethodVisitor, MethodAdapter,
			  Attribute, Label,
			  Opcodes}
import scala.collection.mutable.{ArrayStack, ArrayBuffer, ResizableArray,
				 ListBuffer}
import java.util.IdentityHashMap
import Util.insnListAsIterable

class DataflowGraph extends IdentityHashMap[AbstractInsnNode,Array[DataflowGraph.DataSrc]] {
  def dump(method:MethodNode) {
    for (ins <- method.instructions) {
      System.out.print("* "+ins)
      if (containsKey(ins)) {
	System.out.print(" // ")
	System.out.print(get(ins).mkString(", "))
      }
      System.out.println()
    }
  }
}

/** Creates a data flow graph for a JVM method.
 *  The data flow graph states the possible sources for the different arguments
 *  of instructions.
 *  (Pure data moving instructions are excludedin the output.)
 */
object DataflowGraph {
  protected class State[T <: AnyRef](maxLocals:Int) {
    val stack = new ArrayStack[T]()
    val locals = new Array[T](maxLocals)

    def store(pos:Int, value:T) {locals(pos) = value}
    def load(pos:Int) : T = locals(pos)

    def push(value:T) : Unit = stack.push(value)
    def pop() : T = stack.pop
    def peek : T = stack.peek

    def clear(cleared_value: => T) {
      for (i <- 0 until locals.length) locals(i) = cleared_value;

      val stacksize = stack.size;
      stack.clear;
      for (i <- 0 until stacksize) stack.push(cleared_value);
    }

    override def toString : String = "<locals="+locals.mkString(",")+"  stack="+stack.mkString(",")+">"
  }

  /** Dataflow data source. */
  sealed case class DataSrc();

  /** Data source representing a constant. */
  case class ConstSrc(constant:Object) extends DataSrc;

  /** Data source for a value output from a given instruction. */
  case class InsSrc(node:AbstractInsnNode) extends DataSrc;

  /** Data source representing a method parameter. */
  case class ArgSrc(nr:Int) extends DataSrc;

  /** Data source representing a phi-node: a join-point in the dataflow graph. */
  case class Join(label:Label) extends DataSrc {
    val srcs : ListBuffer[DataSrc] = new ListBuffer()
  }

  /** Data source representing the second part of a long or double value. */
  case class PartTwo(src:DataSrc) extends DataSrc;


  private val INT_MINUS_ONE= new java.lang.Integer(-1)
  private val INT_ZERO	= new java.lang.Integer(0)
  private val INT_ONE	= new java.lang.Integer(1)
  private val INT_TWO	= new java.lang.Integer(2)
  private val INT_THREE	= new java.lang.Integer(3)
  private val INT_FOUR	= new java.lang.Integer(4)
  private val INT_FIVE	= new java.lang.Integer(5)

  private val FLOAT_ZERO= new java.lang.Float(0)
  private val FLOAT_ONE	= new java.lang.Float(1)
  private val FLOAT_TWO	= new java.lang.Float(2)

  private val LONG_ZERO	= new java.lang.Long(0)
  private val LONG_ONE	= new java.lang.Long(1)

  private val DOUBLE_ZERO= new java.lang.Double(0)
  private val DOUBLE_ONE= new java.lang.Double(1)

  def construct(method:MethodNode) = {
    import Opcodes._

    val state = new State[DataSrc](method.maxLocals);
    val nodeDeps = new DataflowGraph();
    var reachable : Boolean = false

    def push_double(value:DataSrc) : Unit = {
      state.push(value);
      state.push(PartTwo(value))
    }

    def pop_double() : DataSrc = {
      state.pop;
      state.pop
    }

    val returnSrcs = new ListBuffer[DataSrc]();
    for (instruction <- method.instructions) {
      instruction match {
	case ins:LabelNode => {
	  state.clear{new Join(ins.getLabel)};
	}

	case ins:InsnNode => {
	  ins.getOpcode match {
	    case ACONST_NULL|ICONST_M1|ICONST_0|ICONST_1|ICONST_2|ICONST_3|ICONST_4|ICONST_5|FCONST_0|FCONST_1|FCONST_2 => {
	      val constant = ins.getOpcode match { // Push single-word
		case ACONST_NULL => null

		case ICONST_M1   => INT_MINUS_ONE
		case ICONST_0	 => INT_ZERO
		case ICONST_1	 => INT_ONE
		case ICONST_2	 => INT_TWO
		case ICONST_3	 => INT_THREE
		case ICONST_4	 => INT_FOUR
		case ICONST_5	 => INT_FIVE

		case FCONST_0	 => FLOAT_ZERO
		case FCONST_1	 => FLOAT_ONE
		case FCONST_2	 => FLOAT_TWO
	      }
	      state.push(ConstSrc(constant));
	    }

	    case LCONST_0|LCONST_1|DCONST_0|DCONST_1 => { // Push double-word
	      val constant = ins.getOpcode match {
		case LCONST_0	 => LONG_ZERO
		case LCONST_1	 => LONG_ONE

		case DCONST_0	 => DOUBLE_ZERO
		case DCONST_1	 => DOUBLE_ONE
	      }
	      push_double(ConstSrc(constant));
	    }

	    // Stack manipulation instructions:
	    case NOP => {}
	    case POP => {state.pop}
	    case POP2 => {state.pop; state.pop}
	    case DUP => {val a=state.peek;
			 state.push(a); }
	    case DUP2 => {val (a,b)=state.stack.preserving{(state.pop,state.peek)};
			 state.push(b);
			 state.push(a);
			}
	    case SWAP => {val a=state.pop;
			  val b=state.pop;
			  state.push(a);
			  state.push(b); }
	    case DUP_X1 => {
	      val (a,b)=(state.pop, state.pop);
	      state.push(a);
	      state.push(b);
	      state.push(a);
	    }
	    case DUP_X2 => {
	      val (a,b,c)=(state.pop, state.pop, state.pop);
	      state.push(a);
	      state.push(c);
	      state.push(b);
	      state.push(a);
	    }
	    case DUP2_X1 => {
	      val (a,b,c)=(state.pop, state.pop, state.pop);
	      state.push(b);
	      state.push(a);
	      state.push(c);
	      state.push(b);
	      state.push(a);
	    }
	    case DUP2_X2 => {
	      val (a,b,c,d)=(state.pop, state.pop, state.pop, state.pop);
	      state.push(b);
	      state.push(a);
	      state.push(d);
	      state.push(c);
	      state.push(b);
	      state.push(a);
	    }

	    case (INEG | FNEG |
		  I2F | I2B | I2S | I2C | F2I) => { // Pop single-word, push single-word
	      val (a)=(state.pop);
	      nodeDeps.put(ins, Array(a))
	      state.push(InsSrc(ins));
	    }

	    case (IADD | ISUB | IMUL | IDIV | IREM |
		  FADD | FSUB | FMUL | FDIV | FREM |
		  IAND | IOR | IXOR |
		  ISHL | ISHR | IUSHR |
		  FCMPL | FCMPG |
		  AALOAD | IALOAD | FALOAD | BALOAD | SALOAD | CALOAD
		) => { // Pop 2, push single-word
	      val (a,b)=(state.pop, state.pop);
	      nodeDeps.put(ins, Array(b,a))
	      state.push(InsSrc(ins));
	    }

	    case (I2D | I2L | F2D) => { // Pop single-word, push double-word
	      val (a)=(state.pop);
	      nodeDeps.put(ins, Array(a))
	      push_double(InsSrc(ins));
	    }

	    case (D2I | D2F | L2I | L2F) => { // Pop double-word, push single-word
	      val (a)=(pop_double);
	      nodeDeps.put(ins, Array(a))
	      state.push(InsSrc(ins));
	    }

	    case (LNEG | DNEG |
		  D2L | L2D) => { // Pop double-word, push double-word
	      val (a)=(pop_double);
	      nodeDeps.put(ins, Array(a))
	      state.push(InsSrc(ins));
	    }

	    case (LCMP | DCMPL | DCMPG) => { // Pop 2 double-words, push single-word
	      val (a,b)=(pop_double, pop_double);
	      nodeDeps.put(ins, Array(b,a))
	      state.push(InsSrc(ins));
	    }

	    case (LALOAD | DALOAD) => { // Pop 2, push double-word
	      val (a,b)=(state.pop, state.pop);
	      nodeDeps.put(ins, Array(b,a))
	      push_double(InsSrc(ins));
	    }

	    case (IASTORE | FASTORE | BASTORE | SASTORE | CASTORE) => { // Pop 3
	      val (a,b,c)=(state.pop, state.pop, state.pop);
	      nodeDeps.put(ins, Array(c,b,a))
	    }

	    case RETURN => {reachable = false;}
	    case (ARETURN | IRETURN | FRETURN) => {
	      val (a) = (state.pop);
	      returnSrcs += a;
	      reachable = false;
	    }
	    case (LRETURN | DRETURN) => {
	      val (a) = (pop_double);
	      returnSrcs += a;
	      reachable = false;
	    }
	  }
	}//format InsnNode

	case ins:VarInsnNode => {
	  ins.getOpcode match {
	    case ALOAD | ILOAD | FLOAD => {
	      state.push(state.load(ins.`var`))
	    }
	    case LLOAD | DLOAD => {
	      push_double(state.load(ins.`var`))
	    }

	    case ASTORE | ISTORE | FSTORE => {
	      state.store(ins.`var`, state.pop)
	    }

	    case LSTORE | DSTORE => {
	      state.store(ins.`var`, pop_double)
	    }
	  }
	}

	case ins:IntInsnNode => {
	  ins.getOpcode match {
	    case BIPUSH|SIPUSH => {
	      val constant = new java.lang.Integer(ins.operand)
	      state.push(ConstSrc(constant));
	    }
	  }
	}

	case ins:FieldInsnNode => {
	  ins.getOpcode match {
	    case PUTSTATIC => { // Pop 1 mixed
	      //TODO: Handle double-word fields!
	      val (a)=(state.pop);
	      nodeDeps.put(ins, Array(a))
	    }

	    case PUTFIELD => { // Pop 1 + 1 mixed
	      //TODO: Handle double-word fields!
	      val (a,b)=(state.pop, state.pop);
	      nodeDeps.put(ins, Array(b,a))
	    }

	    case GETSTATIC => { // Push 1 mixed
	      //TODO: Handle double-word fields!
	      state.push(InsSrc(ins))
	    }

	    case GETFIELD => { // Pop 1, push 1 mixed
	      //TODO: Handle double-word fields!
	      val (a)=(state.pop);
	      nodeDeps.put(ins, Array(a))
	      state.push(InsSrc(ins))
	    }
	  }
	}

	/*
	case ins:MethodInsnNode => {
	  ins.getOpcode match {
	    // INVOKEVIRTUAL; INVOKESPECIAL; INVOKESTATIC; INVOKEINTERFACE
	  }
	}

	case ins:TypeInsnNode => {
	  ins.getOpcode match {
	    // NEW; ANEWARRAY; INSTANCEOF; CHECKCAST
	  }
	}
	*/

	case _:LineNumberNode => {}
      }//match instruction
    }

    nodeDeps
  }// def construct

}

