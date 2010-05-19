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

import java.io.File
import java.util.IdentityHashMap

import org.objectweb.asm.{ClassReader, Type=>AsmType, Opcodes}
import org.objectweb.asm.tree.{ClassNode,MethodNode}

import scala.collection.jcl.Conversions._

object Cro {
  type ObjectState = ObjectAnalysis.ObjectState;

  def reopen[T <: AnyRef](subject:T, staticCls:Class[T]) : T = {
    assert(subject.asInstanceOf[T] != null);
    val dynCls = subject.getClass
    val cl = dynCls.getClassLoader() match {
      case null => ClassLoader.getSystemClassLoader();
      case x => x
    }
     System.err.println("DB| class name: "+dynCls.getName())

    val classFileName = dynCls.getName.replace('.', File.separatorChar)+".class"
    val classUrl = cl.getResource(classFileName)
    val classInput = cl.getResourceAsStream(classFileName)

    val toInclude = new IdentityHashMap[Object,ObjectState]()
    ObjectAnalysis.collectFinalTree(subject, toInclude)
    System.err.println("DB| included objects ("+toInclude.size+"): "+toInclude.keySet)

    val cr = new ClassReader(classInput)
    val replacement = new ClassNode()
    cr.accept(replacement, 0)
    System.err.println("DB| loaded replacement: "+replacement)

    val methods = replacement.methods.asInstanceOf[java.util.List[MethodNode]]

    replacement.accept(new org.objectweb.asm.util.TraceClassVisitor(new java.io.PrintWriter(System.err)))

    for (m <- methods) {
      if ((m.access & Opcodes.ACC_STATIC) == 0)
	{
	  System.out.println("/---- Method "+m.name+" "+m.desc+" ----")
	  val dfg = DataflowGraph.construct(m)
	  dfg.dump(m)
	}
    }

    subject
  }

  def main(args:Array[String]) {
    val subject = {x:Int => x*x}
    reopen(subject, classOf[Function[Int,Int]])

    val c2 = 123;
    val subject2 = {x:Int => x+c2}
    reopen(subject2, classOf[Function[Int,Int]])

    val c3 = args.length;
    val subject3 = {x:Int => x+c3}
    reopen(subject3, classOf[Function[Int,Int]])

    val c4 = {x:Int => y:Int => x+y}
    val subject4 = c4(args.length)
    reopen(subject4, classOf[Function[Int,Int]])
    reopen(c4, classOf[Function[Int,Function[Int,Int]]])
  }
}
