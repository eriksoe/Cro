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

import scala.collection.mutable.{Map => MMap}
import java.util.IdentityHashMap
import java.lang.reflect.{Field,Modifier,AccessibleObject}

object ObjectAnalysis {
  type ObjectState = MMap[(String,Class[_]), Object];

  def collectFinalTree(subject:Object, set:IdentityHashMap[Object,ObjectState]) {
    if (subject==null) return;
    if (set.containsKey(subject)) return;

    testImmutablility(subject) match {
      case Some(fieldMap) => {
	// Include and recurse:
	set.put(subject,fieldMap)

	for (((_fname,ftype),fvalue) <- fieldMap;
	     if (! ftype.isPrimitive))
	  {
	    collectFinalTree(fvalue, set)
	  }
      }

      case None => {} // Not immutable; don't include.
    }
  }

  def testImmutablility(subject:Object) : Option[ObjectState] = {
    val cls = subject.getClass
    val fields = cls.getDeclaredFields

    try {
      AccessibleObject.setAccessible(fields.asInstanceOf[Array[AccessibleObject]], true) } catch {
      // Field not accessible?
      case _:SecurityException => return None;
    }

    val fieldMap : ObjectState = MMap()
    for (f <- fields;
	 val mods = f.getModifiers;
	 if ((mods & Modifier.STATIC) == 0))
      {
	if ((mods & Modifier.FINAL) == 0) return None; // Object not worthy
	if (f.getType.isArray) return None;            // Object not worthy

	val fieldValue = f.get(subject)
	fieldMap += ((f.getName,f.getType) -> fieldValue)
      }

    return Some(fieldMap)
  }

}
