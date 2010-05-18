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

import org.objectweb.asm.tree.{InsnList, AbstractInsnNode};

object Util {

  implicit def insnListAsIterable(insnlist:InsnList) : Iterable[AbstractInsnNode] =
    new Iterable[AbstractInsnNode]() {
      def elements = new Iterator[AbstractInsnNode]() {

	var current = insnlist.getFirst;

	def hasNext = current != null

	def next = {
	  val save = current
	  current = current.getNext
	  save
	}
      }
    }

}
