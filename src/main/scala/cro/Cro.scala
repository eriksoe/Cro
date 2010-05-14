package cro;

import java.lang.reflect.{Field,Modifier,AccessibleObject}
import java.io.File
import java.util.IdentityHashMap
import scala.collection.mutable.{Map => MMap}

import org.objectweb.asm.{ClassReader}
import org.objectweb.asm.tree.{ClassNode}

object Cro {
  type ObjectState = MMap[(String,Class[_]), Object];

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
    collectFinalTree(subject, toInclude)
    System.err.println("DB| included objects: "+toInclude.keySet)

    val cr = new ClassReader(classInput)
    val replacement = new ClassNode()
    cr.accept(replacement, 0)
    System.err.println("DB| loaded replacement: "+replacement)

    subject
  }

  def collectFinalTree(subject:Object, set:IdentityHashMap[Object,ObjectState]) {
    if (subject==null) return;
    if (set.containsKey(subject)) return;

    val cls = subject.getClass
    val fields = cls.getDeclaredFields

    // Debugging:
    System.err.println("DB| collectFinalTree("+subject+"): class="+cls.getName+" fields="+fields.length)
    for (f <- fields) {
      val mods = f.getModifiers
       if ((mods & Modifier.STATIC) == 0) {
	System.err.println("  field: "+f.getType+" "+f.getName+" "+
			   (if ((mods & Modifier.FINAL) != 0) "final" else "non-final")+" "+
			   (if ((mods & Modifier.STATIC) != 0) "static" else "non-static"))
       }
    }

    try {
    AccessibleObject.setAccessible(fields.asInstanceOf[Array[AccessibleObject]], true) } catch {
      case _:SecurityException => return; // Field not accessible - exclude object
    }

    // Debugging:
    for (f <- fields) {
      val mods = f.getModifiers
      if ((mods & Modifier.STATIC) == 0) {
	System.err.println("  field value: "+f.getName+" = "+f.get(subject))
      }
    }

    val fieldMap : ObjectState = MMap()
    for (f <- fields;
	 val mods = f.getModifiers;
	 if ((mods & Modifier.STATIC) == 0))
      {
	if ((mods & Modifier.FINAL) == 0) return; // Object not worthy
	val fieldValue = f.get(subject)
	fieldMap += ((f.getName,f.getType) -> fieldValue)
      }

    // We now know that 'subject' is to be included. Recurse:
    for (((_fname,ftype),fvalue) <- fieldMap;
	 if (! ftype.isPrimitive))
      {
	collectFinalTree(fvalue, set)
      }

    set.put(subject,fieldMap)
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
